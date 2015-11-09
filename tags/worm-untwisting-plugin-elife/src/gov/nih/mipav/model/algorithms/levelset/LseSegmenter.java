package gov.nih.mipav.model.algorithms.levelset;
import java.util.Vector;

/**
 * The abstract base class for segmentation via level-set evolution.
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public abstract class LseSegmenter
{
    //~ Instance fields ------------------------------------------------------

	/** The number of image elements. */
    protected int m_iQuantity;
    
    /** The image elements. */
    protected float[] m_afData;
    
    /**
     * Masks to indicate which image elements are to be processed by the
     * segmenter.
     */
    protected boolean[] m_abMask;

    /**
     * The fast marching object used for region growing and for signed
     * distance transforms.
     */
    protected LseFastMarch m_kFastMarcher;

    /** The interior of the segmented result from region growing. */
    protected int[] m_aiInterior;

    /** The boundary of the segmented result from region growing. */
    protected int[] m_aiBoundary;

    /**
     * The current distance for boundary pixels in the region growing
     * involving the signed distance transform.
     */
    protected float m_fCurrentDistance;
    
    /**
     * The maximum allowed distance for the signed distance transform.
     * This is user-specified.
     */
    protected float m_fMaxDistance;
    
    /**
     * The signed distance transform computed from the coarse-level region
     * grown by the fast marching method. */
    protected float[] m_afSignedDistance;
    
    /**
     * The mask that indicates which image elements have been included in
     * the signed distance transform.
     */
    protected boolean[] m_abSignedDistanceMask;

    /** The level-set evolver. */
    protected LsePdeFilter m_kLevelSetEvolver;

    //~ Constructors ---------------------------------------------------------

    /**
     * Create a new level-set segmenter object.
     * 
     * @param iQuantity The number of image elements.
     * @param afData The image elements.
     * @param abMask The masks to indicate which image elements should be
     *     processed by the segmenter.
     */
    protected LseSegmenter (int iQuantity, float[] afData, boolean[] abMask)
    {
        m_iQuantity = iQuantity;
        m_afData = afData;
        m_abMask = abMask;
        m_kFastMarcher = null;
        m_aiInterior = null;
        m_aiBoundary = null;
        m_fCurrentDistance = 0.0f;
        m_fMaxDistance = 0.0f;
        m_afSignedDistance = new float[m_iQuantity];
        m_abSignedDistanceMask = new boolean[m_iQuantity];
        m_kLevelSetEvolver = null;
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
    public abstract void setPDEParameters (float fBorderValue, int eScaleType,
        float fTimeStep);

    /**
     * Set the number of iterations to use in the nonlinear diffusion
     * (curvature flow filter) applied to the input image.
     * 
     * @param iDIterations The number of iterations.
     */
    public abstract void setDiffusionIterations (int iDIterations);

    /**
     * Set the scale to use in computing the blurred gradient magnitude
     * of the curvature flow image.
     * 
     * @param fScale The scale for the blurred gradient magnitude image.
     */
    public abstract void setGradientMagnitudeScale (float fScale);

    /**
     * Set the parameters for the sigmoid function through which the
     * blurred gradient magnitude image is processed.
     * 
     * @param fAlpha The variance of the function.
     * @param fBeta The center of the function.
     * @param fMin The minimum of the function.
     * @param fMax The maximum of the function.
     */
    public abstract void setSigmoidFilter (float fAlpha, float fBeta, float fMin, float fMax );
    
    /**
     * Get the fast marching object.  This object is used for the coarse-level
     * segmentation and for the signed-distance transform.
     * 
     * @return The fast marching object.
     */
    public final LseFastMarch getFastMarcher ()
    {
        return m_kFastMarcher;
    }

    /**
     * Get the level-set evolver object.
     * 
     * @return The evolver object.
     */
    public final LsePdeFilter getLevelSetEvolver ()
    {
        return m_kLevelSetEvolver;
    }

    //------------------------------------------------------------------------
    // Region Growing (broad-phase segmentation)
    //------------------------------------------------------------------------
    /**
     * The region is grown from the input seeds.  You can apply the region
     * growing multiple times to the same image, each time starting with
     * beginCoarse.  A derived class must implement this function and call
     * the base-class function before it does any of its specialized work.
     * 
     * @param aiSeeds The initial region to grow.
     */
    public void beginCoarse (int[] aiSeeds)
    {
        m_kFastMarcher = null;
        m_aiInterior = null;
        m_aiBoundary = null;
     }

    /**
     * Apply a single iteration of the region-growing fast march.
     */
    public final void iterateCoarse ()
    {
        m_kFastMarcher.iterate();
    }

    /**
     * Call endCoarse when you are finished with the region growing.  After
     * this call, getInterior and getBoundary may be called to examine the
     * coarse-level segmentation.
      */
    public final void endCoarse ()
    {
        Vector kBoundary = new Vector(m_iQuantity/8,m_iQuantity/8);
        Vector kInterior = new Vector(m_iQuantity/8,m_iQuantity/8);
        int i;
        for (i = 0; i < m_iQuantity; i++)
        {
            // Warning:  The order of calls to isBoundary(i) and isInterior(i)
            // is important.
            if (m_kFastMarcher.isBoundary(i))
            {
                kBoundary.add(new Integer(i));
            }
            else if (m_kFastMarcher.isInterior(i))
            {
                kInterior.add(new Integer(i));
            }
        }

        Integer kObject;

        m_aiBoundary = new int[kBoundary.size()];
        for (i = 0; i < m_aiBoundary.length; i++)
        {
            kObject = (Integer)kBoundary.get(i);
            m_aiBoundary[i] = kObject.intValue();
        }

        m_aiInterior = new int[kInterior.size()];
        for (i = 0; i < m_aiInterior.length; i++)
        {
            kObject = (Integer)kInterior.get(i);
            m_aiInterior[i] = kObject.intValue();
        }
    }

    /**
     * The set of interior points that were identified as part of the
     * coarse-level region growing.
     * 
     * @return An array of interior points of the grown region.
     */
    public final int[] getInterior ()
    {
        return m_aiInterior;
    }

    /**
     * The set of boundary points that were identified as part of the
     * coarse-level region growing.
     * 
     * @return An array of boundary points of the grown region.
     */
    public final int[] getBoundary ()
    {
        return m_aiBoundary;
    }
    //------------------------------------------------------------------------

    //------------------------------------------------------------------------
    // Signed-Distance Transform (region for narrow-phase segmentation)
    //------------------------------------------------------------------------
    /**
     * An annulus is computed about the boundary of the coarse-level
     * segmentation.  The maximum distance from annulus points to the
     * boundary is the input to this function (in image-element units).  The
     * function returns 'true' if and only if the set of boundary points from
     * the region growing is not empty.  A derived class must implement this
     * function and call the base-class function before it does any of its
     * specialized work.
     *
     * @param fMaxDistance The maximum distance allowed in the region
     *     growing associated with the signed distance transform.
     * @return 'true' iff the current region has at least one boundary point.
     */
    public boolean beginDistanceTransform (float fMaxDistance)
    {
        m_kFastMarcher = null;
        m_fMaxDistance = (fMaxDistance > 0.0f ? fMaxDistance : 0.0f);
        m_fCurrentDistance = 0.0f;
        for (int i = 0; i < m_iQuantity; i++)
        {
            m_afSignedDistance[i] = 0.0f;
            m_abSignedDistanceMask[i] = false;
        }
        return m_aiBoundary.length > 0;
    }

    /**
     * Apply a single iteration of the annulus construction.
     * 
     * @return 'true' iff there are still iterations to perform.  This is
     *   the case as long as the current distance is smaller than the
     *   maximum specified distance.
     */
    public final boolean iterateDistanceTransform ()
    {
        m_kFastMarcher.iterate();
        m_kFastMarcher.computeTimeExtremes();
        m_fCurrentDistance = m_kFastMarcher.getMaxTime();
        return m_fCurrentDistance < m_fMaxDistance;
    }

    /**
     * Call endDistanceTransform when you are finished with the distance
     * transform construction.  After this call, getSignedDistanceImage and
     * getSignedDistanceMask may be called to examine the annulus points and
     * signed distances.
     */
    public final void endDistanceTransform ()
    {
        // Change signs on the distances associated with the old interior
        // points.  This produces a signed-distance transform.
        int i;
        for (i = 0; i < m_aiInterior.length; i++)
        {
            int j = m_aiInterior[i];
            m_kFastMarcher.setTime(j,-m_kFastMarcher.getTime(j));
        }

        // Record the annulus positions and signed-distance values.
        for (i = 0; i < m_iQuantity; i++)
        {
            float fT = m_kFastMarcher.getTime(i);
            if (Math.abs(fT) != Float.MAX_VALUE)
            {
                m_afSignedDistance[i] = fT;
                m_abSignedDistanceMask[i] = true;
            }
        }
    }

    /**
     * Get the signed distance transform.  These have relevant values only
     * after a call to EndDistanceTransform.  The signed distances are set
     * to zero for image elements that are outside the annulus (masked
     * region).
     * 
     * @return The array of signed distance values.
     */
    public final float[] getSignedDistanceImage ()
    {
        return m_afSignedDistance;
    }

    /**
     * Get the signed distance transform mask.  These have relevant values
     * only after a call to EndDistanceTransform.  The 'true' masks indicate
     * image elements that are part of the computed annulus.
     * 
     * @return The array of signed distance transform masks.
     */
    public final boolean[] getSignedDistanceMask ()
    {
        return m_abSignedDistanceMask;
    }
    //------------------------------------------------------------------------

    //------------------------------------------------------------------------
    // Level-Set Evolution (narrow-phase segmentation)
    //------------------------------------------------------------------------
    /**
     * Evolve the signed-distance transform in the annulus constructed
     * previously.  A derived class must implement this function and call the
     * base-class function before it does any of its specialized work.
     * 
     * @param fAdvectionWeight The advection weight in the PDE governing
     *     the level-set evolution in the annulus.
     * @param fPropagationWeight The propagation weight in the PDE governing
     *     the level-set evolution in the annulus.
     * @param fCurvatureWeight The curvature weight in the PDE governing
     *     the level-set evolution in the annulus.
     * @param fLaplacianWeight The Laplacian weight in the PDE governing
     *     the level-set evolution in the annulus.
     * @param fTimeStep The time step for one iteration of the numerical
     *     PDE solver.
     */
    public void beginEvolution (float fAdvectionWeight,
        float fPropagationWeight, float fCurvatureWeight,
        float fLaplacianWeight, float fTimeStep)
    {
        m_kLevelSetEvolver = null;
    }

    /**
     * Apply a single iteration of the evolution.
     */
    public final void iterateEvolution ()
    {
        m_kLevelSetEvolver.update();
    }

    /**
     * Call endEvolution when you are finished with the level-set evolution.
     */
    public final void endEvolution ()
    {
        // There is nothing to do, but the function exists to maintain
        // consistency in the begin/iterate/end subsystems of this class.
    }
    //------------------------------------------------------------------------
}
