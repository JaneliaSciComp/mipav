package gov.nih.mipav.model.algorithms.levelset;

/**
 * <p>This class is a simple wrapper for the parameters that occur in the
 * level-set evolution, which is controlled by the partial differential
 * equation:
 * <br><br> &nbsp; &nbsp;
 *     du/dt = -a*A(x).grad(u) - b*P(x)*|grad(u)| + c*Z(x)*K(u)*|grad(u)|
 *         + d*Laplacian(u)
 * <br><br>
 * where u(x,t) is the evolved image at time t, du/dt is the time derivative
 * of u, grad(u) is the gradient of u with respect to the x-variable, A(x) is
 * the advection term with advection weight <i>a</i>, P(x) is the propagation
 * term with propagation weight <i>b</i>, Z(x) is the curvature term with
 * curvature weight <i>c</i>, K(u) is the mean curvature of level curves
 * (in 2D) or surfaces (in 3D), and Laplacian(u) is the sum of second-order
 * unmixed derivatives with Laplacian weight <i>d</i>.  The notation
 * A(x).grad(u) indicates the dot product of the two vectors.</p>
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public class LseParameters
{
    //~ Instance fields ------------------------------------------------------

	/** The advection weight 'a'. */
    private float m_fAdvectionWeight;
    
    /** The propagation weight 'b'. */
    private float m_fPropagationWeight;
    
    /** The curvature weight 'c'. */
    private float m_fCurvatureWeight;
    
    /** The Laplacian weight 'd'. */
    private float m_fLaplacianWeight;
    
    /**
     * A small, positive constant added to the gradient magnitude when it is
     * used in a division.  This guards against divisions by zero.
     */
    private float m_fGradientEpsilon;
    
    /**
     * The stability of the numerical PDE solver depends heavily on the data
     * being processed.  The time step must be carefully controlled to be
     * small enough to ensure stability.  These parameters are modified during
     * the PDE solver update to reflect the current state of the image data.
     * The modified values are used in computeTimeStep() to calculate the
     * time step to be used by the next PDE solver update.
     */
    private float
    	m_fMaxAdvPropTimeStep,
    	m_fMaxCurvatureTimeStep,
    	m_fMaxAdvectionChange,
    	m_fMaxPropagationChange,
    	m_fMaxCurvatureChange;

    //~ Constructors ---------------------------------------------------------

    /**
     * Creates a new LseParameters object.
     */
    public LseParameters ()
    {
        m_fAdvectionWeight = 0.0f;
        m_fPropagationWeight = 0.0f;
        m_fCurvatureWeight = 0.0f;
        m_fLaplacianWeight = 0.0f;
        m_fGradientEpsilon = 1e-08f;
        m_fMaxAdvPropTimeStep = 0.25f;
        m_fMaxCurvatureTimeStep = 0.25f;
        m_fMaxAdvectionChange = 0.0f;
        m_fMaxPropagationChange = 0.0f;
        m_fMaxCurvatureChange = 0.0f;
    }

    //~ Methods --------------------------------------------------------------

    /**
     * Set the advection weight 'a'.
     * 
     * @param fAdvectionWeight The desired advection weight.
     */
    public final void setAdvectionWeight (float fAdvectionWeight)
    {
        m_fAdvectionWeight = fAdvectionWeight;
    }

    /**
     * Get the advection weight 'b'.
     * 
     * @return The current advection weight.
     */
    public final float getAdvectionWeight ()
    {
        return m_fAdvectionWeight;
    }

    /**
     * Set the propagation weight 'b'.
     * 
     * @param fPropagationWeight The desired propagation weight.
     */
    public final void setPropagationWeight (float fPropagationWeight)
    {
        m_fPropagationWeight = fPropagationWeight;
    }

    /**
     * Get the propagation weight 'b'.
     * 
     * @return The current propagation weight.
     */
    public final float getPropagationWeight ()
    {
        return m_fPropagationWeight;
    }

    /**
     * Set the curvature weight 'c'.
     * 
     * @param fCurvatureWeight The desired curvature weight.
     */
    public final void setCurvatureWeight (float fCurvatureWeight)
    {
        m_fCurvatureWeight = fCurvatureWeight;
   }

    /**
     * Get the curvature weight 'c'.
     * 
     * @return The current curvature weight.
     */
    public final float getCurvatureWeight ()
    {
        return m_fCurvatureWeight;
    }

    /**
     * Set the Laplacian weight 'd'.
     * 
     * @param fLaplacianWeight The desired Laplacian weight.  The value must
     *     be nonnegative.  If it is positive, the Laplacian term adds some
     *     diffusion to the equation, which tends to stabilize the numerical
     *     calculations.
     */
    public final void setLaplacianWeight (float fLaplacianWeight)
    {
        if (fLaplacianWeight > 0.0f)
        {
            m_fLaplacianWeight = fLaplacianWeight;
        }
        else
        {
            m_fLaplacianWeight = 0.0f;
        }
    }

    /**
     * Get the Laplacian weight 'd'.
     * 
     * @return The current Laplacian weight.
     */
    public final float getLaplacianWeight ()
    {
        return m_fLaplacianWeight;
    }

    /**
     * Set the gradient magnitude epsilon value.  This value guards against
     * a division by z zero-valued gradient magnitude in the curvature
     * calculations.
     * 
     * @param fGradientEpsilon The gradient magnitude epsilon value.  The
     *     number must be positive.
     */
    public final void setGradientEpsilon (float fGradientEpsilon)
    {
        if (fGradientEpsilon > 0.0f)
        {
            m_fGradientEpsilon = fGradientEpsilon;
        }
        else
        {
            m_fGradientEpsilon = 0.0f;
        }
    }

    /**
     * Get The gradient magnitude epsilon value.
     * 
     * @return The current gradient magnitude epsilon value.
     */
    public final float getGradientEpsilon ()
    {
        return m_fGradientEpsilon;
    }

    /**
     * Set the maximum advection-propagation time step.  The default value
     * is 0.25.  At this time, there is no reason to change its value.
     * 
     * @param fMaxAdvPropTimeStep The desired maximum advection-propagation
     *     time step.
     */
    public final void setMaxAdvPropTimeStep (float fMaxAdvPropTimeStep)
    {
        m_fMaxAdvPropTimeStep = fMaxAdvPropTimeStep;
    }

    /**
     * Get the maximum advection-propagation time step.
     * 
     * @return The current maximum advection-propagation time step.
     */
    public final float getMaxAdvPropTimeStep ()
    {
        return m_fMaxAdvPropTimeStep;
    }

    /**
     * Set the maximum curvature time step.  The default value is 0.25.  At
     * this time, there is no reason to change its value.
     * 
     * @param fMaxCurvatureTimeStep The desired maximum curvature time step.
     */
    public final void setMaxCurvatureTimeStep (float fMaxCurvatureTimeStep)
    {
        m_fMaxCurvatureTimeStep = fMaxCurvatureTimeStep;
    }

    /**
     * Get the maximum curvature time step.
     * 
     * @return The current maximum curvature time step.
     */
    public final float getMaxCurvatureTimeStep ()
    {
        return m_fMaxCurvatureTimeStep;
    }

    /**
     * Set the maximum advection change allowed in the PDE solver update.
     * 
     * @param fMaxAdvectionChange The desired maximum advection change.
     */
    public final void setMaxAdvectionChange (float fMaxAdvectionChange)
    {
        m_fMaxAdvectionChange = fMaxAdvectionChange;
    }

    /**
     * Get the maximum advection change allowed in the PDE solver update.
     * 
     * @return The current maximum advection change.
     */
    public final float getMaxAdvectionChange ()
    {
        return m_fMaxAdvectionChange;
    }

    /**
     * Set the maximum propagation change allowed in the PDE solver update.
     * 
     * @param fMaxPropagationChange The desired maximum propagation change.
     */
    public final void setMaxPropagationChange (float fMaxPropagationChange)
    {
        m_fMaxPropagationChange = fMaxPropagationChange;
    }

    /**
     * Get the maximum propagation change allowed in the PDE solver update.
     * 
     * @return The current maximum propagation change.
     */
    public final float getMaxPropagationChange ()
    {
        return m_fMaxPropagationChange;
    }

    /**
     * Set the maximum curvature change allowed in the PDE solver update.
     * 
     * @param fMaxCurvatureChange The desired maximum curvature change.
     */
    public final void setMaxCurvatureChange (float fMaxCurvatureChange)
    {
        m_fMaxCurvatureChange = fMaxCurvatureChange;
    }

    /**
     * Get the maximum curvature change allowed in the PDE solver update.
     * 
     * @return The current maximum curvature change.
     */
    public final float getMaxCurvatureChange ()
    {
        return m_fMaxCurvatureChange;
    }

    /**
     * Compute the time step for the next PDE solver update.  The time step
     * is based on the current state of the system that is represented by
     * the members of this class.  This state is modified in the LseEvolve2
     * function computeFunction(x,y) and in the LseEvolve3 member function
     * computeFunction(x,y,z).
     * 
     * @return The next time step to be used by the PDE solver update.
     */
    public final float computeTimeStep ()
    {
        float fTimeStep;

        float fAdvPropChange = m_fMaxAdvectionChange + m_fMaxPropagationChange;

        if (m_fMaxCurvatureChange > 0.0f)
        {
            if (m_fMaxAdvectionChange > 0.0f)
            {
                float fTmp0 = m_fMaxAdvPropTimeStep/fAdvPropChange;
                float fTmp1 = m_fMaxCurvatureTimeStep/m_fMaxCurvatureChange;
                fTimeStep = (fTmp0 <= fTmp1 ? fTmp0 : fTmp1);
            }
            else
            {
                fTimeStep = m_fMaxCurvatureTimeStep/m_fMaxCurvatureChange;
            }
        }
        else
        {
            if (m_fMaxAdvectionChange > 0.0f)
            {
                fTimeStep = m_fMaxAdvPropTimeStep/fAdvPropChange;
            }
            else
            {
                fTimeStep = 0.0f;
            }
        }

        m_fMaxAdvectionChange = 0.0f;
        m_fMaxPropagationChange = 0.0f;
        m_fMaxCurvatureChange = 0.0f;

        return fTimeStep;
    }
}
