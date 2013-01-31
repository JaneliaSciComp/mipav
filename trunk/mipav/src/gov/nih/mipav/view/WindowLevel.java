package gov.nih.mipav.view;

import gov.nih.mipav.model.structures.*;


/**
 * The WindowLevel class provides mouse-driven window-level controls for the
 * ModelLUT or ModelRGB lookup tables.  The WindowLevel class can be used to
 * attach mouse-control or any user-interface control to both the ModelLUT and
 * ModelRGB classes.
 *
 * @see ViewJComponentEditImage.java
 * @see PlaneRender.java
 */
public class WindowLevel
{

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Member variables used to adjust the winow and level (contrast and
     * bringtness) by dragging with the right-mouse button:.
     */
    private float[] m_afXWin = new float[4];

    /**
     * Member variables used to adjust the winow and level (contrast and
     * bringtness) by dragging with the right-mouse button:.
     */
    private float[] m_afYWin = new float[4];

    /** image max value */
    private float m_fMax = Float.MIN_VALUE;
    /** image min value */
    private float m_fMin = Float.MAX_VALUE;

    /** previous mouse x-position */
    private float m_fOldX;

    /** previous mouse y-position */
    private float m_fOldY;

    /** Default alpha blending value: */
    private float m_fAlpha = 0.5f;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     */
    public WindowLevel() {}

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean memory.
     */
    public void disposeLocal()
    {
        m_afXWin = null;
        m_afYWin = null;
    }


    /**
     * updateWinLevel updates the window-level for the input lookup table
     * based on two normalized parameters (fX, fY). These parameters may be
     * derived from a normalized x,y mouse position, from slider values, or
     * from any variable.
     *
     * In ViewJComponentEditImage and PlaneRender classesL If the right mouse
     * button is pressed and dragged. updateWinLevel updates the HistoLUT
     * window and level (contrast and brightness) for the
     * ViewJComponentEditImage and PlaneRender classes. The input parameters
     * fX and fY must be in normalized screen space (0-1).
     *
     *
     * @param fX the normalized window parameter (0-1)
     * @param fY the normalized level parameter (0-1)
     * @param bFirstUpdate when true initialize the WindowLevel function
     * @param kLookupTable either the ModelLUT or the ModelRGB being modified
     * @param kImage the ModelImage the lookup table describes. 
     * @return true when the lookup table changes, false when no change
     */
    public boolean updateWinLevel( float fX, float fY, boolean bFirstUpdate,
                                   ModelStorageBase kLookupTable,
                                   ModelImage kImage )
    {
        /* If this is the first time the kLookupTable is updated for
         * window-level contrl, setup the member variables to change the
         * HistoLUT. */
        if (bFirstUpdate)
        {
            if ( kImage.isColorImage() )
            {
                initWinLevelRGB( (ModelRGB)kLookupTable, kImage );
            }
            else
            {
                initWinLevelGray( (ModelLUT)kLookupTable, kImage );
            }
            
            /* Keep track if the mouse position changed: */
            m_fOldX = fX;
            m_fOldY = fY;
        }
        /* Updating window-level has been initialized on the previous call,
         * this changes the HistoLUT: */
        else if ((kImage != null) && (kLookupTable != null) &&
                 ((m_fOldX != fX) || (m_fOldY != fY)))
        {
            /* Determine the HistoLUT window image size based on the
             * ModelImage: */
            float fMinImageWin = m_fMin;
            float fMaxImageWin = m_fMax;

            /* The new window value is based on the fX parameter: */
            float fWindow = 2.0f * fX * (fMaxImageWin - fMinImageWin);

            if (fWindow > (2.0f * (fMaxImageWin - fMinImageWin))) {
                fWindow = 2.0f * (fMaxImageWin - fMinImageWin);
            } else if (fWindow < 0) {
                fWindow = 0;
            }

            /* The new level value is based on the fY parameter: */
            float fLevel = fY * (fMaxImageWin - fMinImageWin);

            if ( fLevel > fMaxImageWin) {
                fLevel = fMaxImageWin;
            } else if ( fLevel < fMinImageWin) {
                fLevel = fMinImageWin;
            }
            
            
            //System.out.println("flevel is " + fLevel);
            //System.out.println("fwindow is " + fWindow); 

            /* The new x positions, and y positions of the middle points on
             * the transfer line: */
            m_afXWin[2] = fLevel + (fWindow / 2.0f);
            m_afXWin[1] = fLevel - (fWindow / 2.0f);
            m_afYWin[2] = m_afYWin[3];
            m_afYWin[1] = m_afYWin[0];

            if (m_afXWin[2] > fMaxImageWin) {
                m_afYWin[2] = 255.0f * (m_afXWin[2] - fMaxImageWin) / fWindow;

                if (m_afYWin[2] > 255.0f) {
                    m_afYWin[2] = 255.0f;
                }
                m_afXWin[2] = fMaxImageWin;
            }

            if (m_afXWin[1] < fMinImageWin) {
                m_afYWin[1] = 255.0f - (255.0f * (fMinImageWin - m_afXWin[1]) / fWindow);

                if (m_afYWin[1] < 0.0f) {
                    m_afYWin[1] = 0.0f;
                }
                m_afXWin[1] = fMinImageWin;
            }
            
            if ( kImage.isColorImage() )
            {
                updateWinLevelRGB( (ModelRGB)kLookupTable, kImage, m_afXWin, m_afYWin );
            }
            else
            {
                updateWinLevelGray( (ModelLUT)kLookupTable, kImage, m_afXWin, m_afYWin );
            }

            /* Store old change in fX,fY positions: */
            m_fOldX = fX;
            m_fOldY = fY;
            
            return true;
        }
        return false;
    }
    
    /** Sets alpha for the notifyWindowDisplayListeners call
     * @param fAlpha the alpha value for blending between images. Needed to
     * pass to the notifyWindowDisplayListeners function.
     */
    public void setAlpha( float fAlpha )
    {
        m_fAlpha = fAlpha;
    }

    /**
     * initWinLevelRGB, initializes the ModelRGB for window-level changes. The
     * transfer function is set to have four control points, and is reset to
     * the default linear.
     * @param kRGBT the ModelRGB to be initialized
     * @param kImage the ModelImage that the min/max values are derived from
     * for initializing the ModelRGB.
     */
    private void initWinLevelRGB( ModelRGB kRGBT,
                                  ModelImage kImage )
    {
        m_fMin = (float)Math.min( kImage.getMinR(), kImage.getMinG() );
        m_fMin = (float)Math.min( m_fMin, kImage.getMinB() );
        m_fMax = (float)Math.max( kImage.getMaxR(), kImage.getMaxG() );
        m_fMax = (float)Math.max( m_fMax, kImage.getMaxB() );
        if ( kImage.getType() == ModelStorageBase.ARGB ) {
            m_afXWin[1] = m_fMin;
            m_afXWin[2] = m_fMax;
        }
        else {
            m_afXWin[1] = m_fMin * 255 / m_fMax;
            m_afXWin[2] = 255;
        }
        m_afXWin[0] = 0;
        m_afXWin[3] = 255;
        
        m_afYWin[0] = 255;
        m_afYWin[1] = 255;
        m_afYWin[2] = 0;
        m_afYWin[3] = 0;

        updateWinLevelRGB( kRGBT, kImage, m_afXWin, m_afYWin );
    }

    /** 
     * initWinLevelGray, initializes the ModelLUT for gray-scale images before
     * window-level operations. The transfer function is set to have four
     * control points, and is reset to the default linear.
     * @param kLUT the ModelLUT to be initialized.
     * @param kImage the ModelImage attached to kLUT
     */
    private void initWinLevelGray( ModelLUT kLUT,
                                   ModelImage kImage )
    {
        m_fMin = (float) kImage.getMin();
        m_fMax = (float) kImage.getMax();
        
        if ( kImage.getType() == ModelStorageBase.UBYTE ) {
            m_fMin = 0;
            m_fMax = 255;
        } else if ( kImage.getType() == ModelStorageBase.BYTE ) {
            m_fMin = -128;
            m_fMax = 127;
        }

        /* Reset the transferline: */
        if ((kImage != null) && (kLUT != null))
        {
            kLUT.resetTransferLine(m_fMin, m_fMax);
            kLUT.getTransferFunction().exportArrays(m_afXWin, m_afYWin);
            
            m_afXWin[1] = m_afXWin[0];
            m_afXWin[2] = m_afXWin[3];
            m_afYWin[1] = m_afYWin[0];
            m_afYWin[2] = m_afYWin[3];
            
            updateWinLevelGray( kLUT, kImage, m_afXWin, m_afYWin);
        }
    }

    /**
     * updateWinLevelRGB, updates the ModelRGB with the new transfer
     * functions. Updates depend on the activation of the different rgb
     * functions, so if the getROn returns false the red function is not
     * updated.
     * @param kRGBT the ModelRGB being updated
     * @param kImage the ModelImage that the ModelRGB describes
     * @param afXWin the x-transfer function
     * @param afYWin the y-transfer function
     */
    private void updateWinLevelRGB( ModelRGB kRGBT, ModelImage kImage,
                                    float[] afXWin, float[] afYWin )
    {
        if ( kRGBT.getROn() )
        {
            kRGBT.getRedFunction().importArrays( afXWin, afYWin, 4 );
        }
        if ( kRGBT.getGOn() )
        {
            kRGBT.getGreenFunction().importArrays( afXWin, afYWin, 4 );
        }
        if ( kRGBT.getBOn() )
        {
            kRGBT.getBlueFunction().importArrays( afXWin, afYWin, 4 );
        }
        kRGBT.makeRGB( -1 );
        kImage.notifyImageDisplayListeners( true, kRGBT );
        if (kImage.getHistogramFrame() != null) {
            kImage.getHistogramFrame().redrawFrames();
        }
        
    }

    /**
     * updateWinLevelGray, updates the ModelLUT with the new transfer
     * function. 
     * @param kLUT the ModelLUT being updated
     * @param kImage the ModelImage that the ModelLUT describes
     * @param afXWin the x-transfer function
     * @param afYWin the y-transfer function
     */
    private void updateWinLevelGray( ModelLUT kLUT, ModelImage kImage,
                                     float[] afXWin, float[] afYWin )
    {
        /* Update the HistoLUT and the renderers: */
        kLUT.getTransferFunction().importArrays( afXWin, afYWin, 4 );
        kImage.notifyImageDisplayListeners( kLUT, true );
        if (kImage.getHistogramFrame() != null) {
            kImage.getHistogramFrame().redrawFrames();
        }
    }
}
