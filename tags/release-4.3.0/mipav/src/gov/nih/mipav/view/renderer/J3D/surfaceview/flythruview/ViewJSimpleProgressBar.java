package gov.nih.mipav.view.renderer.J3D.surfaceview.flythruview;


import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * Wraps the ViewJProgressBar instance which allows an arbitrary minimum and maximum value to be specified for the ends
 * of the progress bar.
 */
public class ViewJSimpleProgressBar extends ViewJProgressBar {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8035459499734828360L;

    /**
     * This is the actual resolution of progress bar which we will be using regardless of the actual range specified.
     */
    private static final int RESOLUTION = 100;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float m_fRangeMax;

    /**
     * Remember the range of input values for the progress bar. It is not required that the minimum be less than the
     * maximum.
     */
    private float m_fRangeMin;

    /**
     * Remember the value passed to the setValue method of the JProgressBar. When valid, should be in range
     * [0,RESOLUTION).
     */
    private int m_iLastValue;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a collection of controls for displaying progress.
     *
     * @param  kTitle        String which appears in the title bar of the frame.
     * @param  kDescription  String which appears in the description field.
     */
    public ViewJSimpleProgressBar(String kTitle, String kDescription) {
        super(kTitle, kDescription, 0, RESOLUTION, false, null, null);

        // Position in the middle of the screen.
        Dimension kScreenSize = Toolkit.getDefaultToolkit().getScreenSize();
        setLocation(kScreenSize.width / 2, kScreenSize.height / 2);
        setVisible(true);

        // Clear the progress bar.
        setRange(0, 1);
        setValue(0);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Called to cleanup any dynamically allocated data members.
     */
    public void dispose() {
        super.dispose();
    }

    /**
     * Called by the garbage collector.
     */
    public void finalize() {
        dispose();
    }

    /**
     * Change the range of integral values for the progress bar. Also resets the current amount of progress to the
     * minimum value specified, which is effectively 0% complete.
     *
     * @param  iMin  integer minimum limit for progress bar
     * @param  iMax  integer maximum limit for progress bar
     */
    public void setRange(int iMin, int iMax) {
        setRange((float) iMin, (float) iMax);
    }

    /**
     * Change the range of floating point values for the progress bar. Also resets the current amount of progress to the
     * minimum value specified, which is effectively 0% complete.
     *
     * @param  fMin  floating point minimum limit for progress bar
     * @param  fMax  floating point maximum limit for progress bar
     */
    public void setRange(float fMin, float fMax) {
        m_iLastValue = -1;
        m_fRangeMin = fMin;
        m_fRangeMax = fMax;
        setValue(fMin);
    }

    /**
     * Called to set the current progress bar indicator position.
     *
     * @param  iValue  integer which should be within the min/max limits of the progress bar previously specified with
     *                 by calling setRange method.
     */
    public void setValue(int iValue) {
        setValue((float) iValue);
    }

    /**
     * Called to set the current progress bar indicator position.
     *
     * @param  fValue  floating point value which should be within the min/max limits of the progress bar previously
     *                 specified with by calling setRange method.
     */
    public void setValue(float fValue) {

        // Map the input value to the range of values for the progress bar.
        // If the range is 0, then force the progress bar to 100%.
        int iMappedValue = RESOLUTION;

        if (m_fRangeMin != m_fRangeMax) {
            float fRange = m_fRangeMax - m_fRangeMin;
            iMappedValue = Math.round(((fValue - m_fRangeMin) * RESOLUTION) / fRange);

            if (iMappedValue < 0) {
                iMappedValue = 0;
            } else if (iMappedValue >= RESOLUTION) {
                iMappedValue = RESOLUTION - 1;
            }
        }

        // Only update the progress bar if the value written
        // to it would change.
        if (iMappedValue != m_iLastValue) {
            m_iLastValue = iMappedValue;
            super.updateValue(iMappedValue, false);
        }
    }
}
