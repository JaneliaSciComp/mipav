package gov.nih.mipav.view;


import java.awt.*;

import javax.swing.*;


/**
 * <p>Title: JPanelProgressBar</p>
 *
 * <p>Description: simple jpanel containing a JProgressBar (similar to ViewJProgressBar but in a panel, not a frame)</p>
 *
 * @author   blink
 * @version  1.0
 */

public class JPanelProgressBar extends JPanel implements ProgressBarInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6963733511040621440L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Actual bar which fills with color as the percentage of completion increases. */
    private JProgressBar pBar;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JPanelProgressBar object.
     */
    public JPanelProgressBar() { }

    /**
     * Creates a new JPanelProgressBar object.
     *
     * @param  min  int
     * @param  max  int
     */
    public JPanelProgressBar(int min, int max) {

        setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.weighty = 1;

        pBar = new JProgressBar();
        pBar.setMinimum(min);
        pBar.setMaximum(max);
        pBar.setValue(0);

        add(pBar, gbc);

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  message  DOCUMENT ME!
     */
    public void appendMessage(String message) { }

    /**
     * DOCUMENT ME!
     */
    public void dispose() { }

    /**
     * DOCUMENT ME!
     */
    public void finalize() { }

    /**
     * Get the progress bar.
     *
     * @return  pBar the progress bar.
     */
    public JProgressBar getProgressBar() {
        return pBar;
    }

    /**
     * Used to get the present value of the progress bar.
     *
     * @return  Value of progress bar.
     */
    public int getValue() {
        return pBar.getValue();
    }

    /**
     * Used to determine if the progress bar is at 100%.
     *
     * @return  <code>true</code> if progress bar is at 100%, <code>false</code> if not.
     */
    public boolean isComplete() {
        return (pBar.getPercentComplete() == 1);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  message  DOCUMENT ME!
     */
    public void setMessage(String message) { }

    /**
     * DOCUMENT ME!
     *
     * @param  title  DOCUMENT ME!
     */
    public void setTitle(String title) { }

    /**
     * Used to set the present value of the progress bar. Changes the percentage label and title of the frame as well.
     *
     * @param  value  Set the progress bar to the given value.
     */
    public void setValue(int value) {
        updateValue(value);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  value           DOCUMENT ME!
     * @param  separateThread  DOCUMENT ME!
     */
    public void setValue(int value, boolean separateThread) {
        updateValue(value, separateThread);
    }

    /**
     * Used to set the present value of the progress bar. Changes the percentage label and title of the frame as well.
     *
     * @param  value  Set the progress bar to the given value.
     */
    public void setValueImmed(int value) {
        updateValueImmed(value);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  vis  DOCUMENT ME!
     */
    public void setVisible(boolean vis) { }

    /**
     * Used to set the present value of the progress bar. Changes the percentage label and title of the frame as well.
     *
     * @param  value  Set the progress bar to the given value.
     */
    public void updateValue(int value) {
        pBar.setValue(value);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  value           DOCUMENT ME!
     * @param  separateThread  DOCUMENT ME!
     */
    public void updateValue(int value, boolean separateThread) {
        pBar.setValue(value);

        if (!separateThread) {
            update(this.getGraphics());
        }
    }

    /**
     * Used to set the present value of the progress bar. Changes the percentage label and title of the frame as well.
     * It also forces an immediate update of the frame. Typically used when using IO functions.
     *
     * @param  value  Set the progress bar to the given value.
     */
    public void updateValueImmed(int value) {
        pBar.setValue(value);
        update(this.getGraphics());
    }

}
