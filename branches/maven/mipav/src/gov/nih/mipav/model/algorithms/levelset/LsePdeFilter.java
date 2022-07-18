package gov.nih.mipav.model.algorithms.levelset;

/**
 * The abstract base class for finite-difference-based solvers for partial
 * differential equations.  This system exists to support level-set evolution
 * for image segmentation.
 *
 * @version  0.1 November 7, 2006
 * @author   David Eberly
 */

public abstract class LsePdeFilter
{
    //~ Static fields/initializers -------------------------------------------

	/** Scale type: no scaling. */
    public static final int NONE = 0;

    /** Scale type: d' = (d-min)/(max-min) in [0,1] */
    public static final int UNIT = 1;

    /** Scale type: d' = -1 + 2*(d-min)/(max-min) in [-1,1] */
    public static final int SYMMETRIC = 2;

    /** Scale type:
     * max > -min:  d' = d/max in [min/max,1]
     * max < -min:  d' = -d/min in [-1,-max/min]
     */
    public static final int PRESERVE_ZERO = 3;

    //~ Instance fields ------------------------------------------------------

    /** The number of image elements. */
    protected int m_iQuantity;

    /** When set to Float.MAX_VALUE, Neumann conditions are in use
     *  (zero-valued derivatives on the image border).  Dirichlet conditions
     *  are used, otherwise (image is constant on the border).
     */
    protected float m_fBorderValue;

    /** The type of image scaling used during filter construction. */
    protected int m_eScaleType;
    
    /** The minimum image value. */
    protected float m_fMin;
    
    /** The offset used in scaling the image data. */
    protected float m_fOffset;
    
    /** The scale factor used in scaling the image data. */
    protected float m_fScale;

    /** The time step for the PDE solver.  The stability of an algorithm
     *  depends on the magnitude of the time step but the magnitude itself
     *  depends on the algorithm.
     */
    protected float m_fTimeStep;
    
    //~ Constructors ---------------------------------------------------------

    /**
     * Creates a new PDE filter object.
     * 
     * @param iQuantity The number of image elements.
     * @param afData The image elements, stored in lexicographical order.
     * @param fBorderValue Specifies how to handle the image value.  When set
     *     to Float.MAX_VALUE, Neumann conditions are in use, in which case
     *     zero-valued derivatives are assumed on the image border.  Otherwise,
     *     Dirichlet conditions are used, in which case the image is assumed
     *     to be constant on the border with value specified by fBorderValue.
     * @param eScaleType The type of scaling to apply to the input image.
     *     The choices are NONE, UNIT, SYMMETRIC, or PRESERVE_ZERO.
     */
    protected LsePdeFilter (int iQuantity, float[] afData, float fBorderValue,
        int eScaleType)
    {
        // assert: afData is not null

        m_iQuantity = iQuantity;
        m_fBorderValue = fBorderValue;
        m_eScaleType = eScaleType;
        m_fTimeStep = 0.0f;

        // Compute the extreme values of the image data.
        float fMax = afData[0];
        m_fMin = fMax;
        for (int i = 1; i < m_iQuantity; i++)
        {
            float fValue = afData[i];
            if (fValue < m_fMin)
            {
                m_fMin = fValue;
            }
            else if (fValue > fMax)
            {
                fMax = fValue;
            }
        }

        // Compute the scale transformation parameters.
        if (m_fMin != fMax)
        {
            switch (m_eScaleType)
            {
            case NONE:
            	m_fMin = 0.0f;
                m_fOffset = 0.0f;
                m_fScale = 1.0f;
                break;
            case UNIT:
                m_fOffset = 0.0f;
                m_fScale = 1.0f/(fMax - m_fMin);
                break;
            case SYMMETRIC:
                m_fOffset = -1.0f;
                m_fScale = 2.0f/(fMax - m_fMin);
                break;
            case PRESERVE_ZERO:
                m_fOffset = 0.0f;
                m_fScale = (fMax >= -m_fMin ? 1.0f/fMax : -1.0f/m_fMin);
                m_fMin = 0.0f;
                break;
            }
        }
        else
        {
        	if (m_eScaleType == NONE)
        	{
        		m_fMin = 0.0f;
        	}
            m_fOffset = 0.0f;
            m_fScale = 1.0f;
        }
    }

    //~ Methods --------------------------------------------------------------

    /**
     * Get the number of image elements.
     * 
     * @return The number of image elements.
     */
    public final int getQuantity ()
    {
        return m_iQuantity;
    }

    /**
     * Get the image border value.
     * 
     * @return The image border value.
     */
    public final float getBorderValue ()
    {
        return m_fBorderValue;
    }

    /**
     * Get the type of image scaling.
     * 
     * @return The type of image scaling.
     */
    public final int getScaleType ()
    {
        return m_eScaleType;
    }

    /**
     * Set the time step for the PDE solver.
     *  
     * @param fTimeStep The time step for the PDE solver.  It must be a
     *     positive number.  Its maximum-allowed size depends on the specific
     *     algorithm.
     */
    public final void setTimeStep (float fTimeStep)
    {
        m_fTimeStep = fTimeStep;
    }

    /**
     * Get the time step for the PDE solver.
     *  
     * @return The time step for the PDE solver.
     */
    public final float getTimeStep ()
    {
        return m_fTimeStep;
    }

    /**
     * The entry point to updating the PDE solver.  This function calls
     * OnPreUpdate, OnUpdate, and OnPostUpdate, in that order.
     */
    public void update ()
    {
        onPreUpdate();
        onUpdate();
        onPostUpdate();
    }

    /**
     * An abstract function that allows derived classes to implement any
     * desired behavior before the PDE solver applies its update function.
     * The derived classes for 2D and 3D implement this to recompute the
     * boundary values when Neumann conditions are used.  If derived classes
     * built on top of the 2D or 3D classes implement this also, they must
     * call the base-class OnPreUpdate first.
     */
    protected abstract void onPreUpdate ();

    /**
     * An abstract function for the update function of the PDE solver.  The
     * derived classes for 2D and 3D implement this to iterate over the image
     * elements, updating an element only if it is not masked out.
     */
    protected abstract void onUpdate ();

    /**
     * An abstract function that allows derived classes to implement any
     * desired behavior after the PDE solver applies its update function.
     * The derived classes for 2D and 3D implement this to swap the buffers
     * for the next pass.  If derived classes built on top of the 2D or 3D
     * classes implement this also, they must call the base-class
     * OnPostUpdate last.
     */
    protected abstract void onPostUpdate ();
}
