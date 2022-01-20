package gov.nih.mipav.view.renderer;


import gov.nih.mipav.view.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.border.*;


/**
 * This class is the base for all the other dialogs. It has two important functions that are used by almost all the
 * dialogs. It also implements all the listeners except for the action listener.
 *
 * @version  1.0 Aug 1, 1998
 * @author   Matthew J. McAuliffe, Ph.D. ( Primary )
 * @author   Ruida Cheng
 */
public abstract class JPanelRendererBase extends JPanel
        implements KeyListener, ActionListener, FocusListener, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7175361900854772739L;

    /** REPLACE indicates the current image is replaced after algorithm is run. */
    protected static final int REPLACE = 0;

    /** NEW indicates a new image is created after the algorithm is run. */
    protected static final int NEW = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Apply button is used to apply the setting of the dialog. */
    protected JButton applyButton;

    /**
     * Cancel button is used on most dialogs. Defining it in the base allows default actions if the user presses return
     * and the button is in focus.
     */
    protected JButton cancelButton;

    /** Flag indicating if the dialog had been cancelled or not. */
    protected boolean cancelFlag;

    /** Close button is used to close the dialog. */
    protected JButton closeButton;

    /**
     * Help button is used on most dialogs. Defining it in the base allows default actions if the user presses return
     * and the button is in focus.
     */
    protected JButton helpButton;

    /** DOCUMENT ME! */
    protected boolean isActiveImage = true;

    /** The main control. */
    protected JPanel mainPanel = null;

    /**
     * OK button is used on most dialogs. Defining it in the base allows default actions if the user presses return and
     * the button is in focus.
     */
    protected JButton OKButton;

    /** Flag indicating if the algorithm should run in a separate thread. Default is <code>true</code>. */
    protected boolean runInSeparateThread = true;

    /** Fonts, same as <code>MipavUtil.font12</code> and <code>MipavUtil.font12B.</code> */
    protected Font serif12, serif12B;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor that sets the parent frame of the dialog and whether or not the dialog is modal. Also adds this as a
     * window listener and key listener to all dialogs.
     *
     * @param  parent  Parent frame.
     */
    public JPanelRendererBase() {
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;
        addKeyListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Action performed method.
     *
     * @param  event  ActionEvent
     */
    public abstract void actionPerformed(ActionEvent event);


    /**
     * Each panel class extends their own get the main control panel method.
     *
     * @return  JPanel main control panel
     */
    public abstract JPanel getMainPanel();


    /**
     * Clear memory.
     */
    public void disposeLocal() {

        /** The main control. */
        mainPanel = null;
        OKButton = null;
        cancelButton = null;
        closeButton = null;
        applyButton = null;
        helpButton = null;
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void focusGained(FocusEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void focusLost(FocusEvent event) { }

    /**
     * Accessor that returns whether or not the dialog has been cancelled.
     *
     * @return  <code>true</code> indicates cancelled, <code>false</code> indicates not cancelled.
     */
    public boolean isCancelled() {
        return cancelFlag;
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) { }

    /**
     * Unchanged.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * Unchanged.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyReleased(KeyEvent e) { }

    /**
     * OK or Cancel should be in focus and if the return key pressed the appropriate action takes place.
     *
     * @param  e  The event that triggers the function.
     */
    public void keyTyped(KeyEvent e) {

        // int x = (int)e.getKeyChar();
        try {

            if (((int) e.getKeyChar() == KeyEvent.VK_ENTER) || ((int) e.getKeyChar() == KeyEvent.VK_SPACE)) {

                // if (x == 10){   // enter key?
                if (OKButton.hasFocus()) {
                    actionPerformed(new ActionEvent(OKButton, 1, "OK"));
                } else if (cancelButton.hasFocus()) {
                    actionPerformed(new ActionEvent(cancelButton, 1, "Cancel"));
                } else if (helpButton.hasFocus()) {
                    actionPerformed(new ActionEvent(helpButton, 1, "Help"));
                }
            }
        } catch (NullPointerException npe) {
            Preferences.debug("NullPointerException caught in JDialogBase.keyTyped(KeyEvent).  No button has focus?\n",
                              4);
        }
    }

    /**
     * Makes a string of a floating point number with a specific number of decimal points.
     *
     * @param   number  Number to be converted to a string.
     * @param   decPts  The number of decimal points.
     *
     * @return  String representation of the number.
     */
    public String makeString(float number, int decPts) {

        int index = String.valueOf(number).indexOf(".");
        int length = String.valueOf(number).length();

        if ((index + decPts) < length) {
            return (String.valueOf(number).substring(0, index + decPts + 1));
        } else {
            return (String.valueOf(number));
        }
    }

    /**
     * Resize the control panel when mouse drag expanding or minimizing the frame.
     *
     * @param  panelWidth   width
     * @param  frameHeight  height
     */
    public void resizePanel(int panelWidth, int frameHeight) { }

    /**
     * Sets the left-hand coordinate flag. If true, change matrix to the left-hand coordinate system.
     *
     * @param  leftHandSys  DOCUMENT ME!
     */
    public void setLeftHandSystem(boolean leftHandSys) { }

    /**
     * Makes the dialog visible in center of screen.
     *
     * @param  status  Flag indicating if the dialog should be visible.
     */
    public void setVisible(boolean status) {
        Rectangle dialogBounds = getBounds();

        this.setLocation((Toolkit.getDefaultToolkit().getScreenSize().width / 2) - (dialogBounds.width / 2),
                         (Toolkit.getDefaultToolkit().getScreenSize().height / 2) - (dialogBounds.height / 2));

        super.setVisible(status);
    }

    /**
     * Makes the dialog visible by calling super method. No location set.
     *
     * @param  status  Flag indicating if the dialog should be visible.
     */
    public void setVisibleStandard(boolean status) {
        super.setVisible(status);
    }

    /**
     * Sets the world coordinate flag. If true, change matrix to the world coordinate system.
     *
     * @param  wcSys  DOCUMENT ME!
     */
    public void setWCSystem(boolean wcSys) { }

    /**
     * Builds the cancel button. Sets it internally as well
     *
     * @return  applyButton the apply button
     */
    protected JButton buildApplyButton() {
        applyButton = new JButton("Apply");
        applyButton.addActionListener(this);
        applyButton.setToolTipText("Apply settings.");
        applyButton.setMinimumSize(MipavUtil.defaultButtonSize);
        applyButton.setPreferredSize(MipavUtil.defaultButtonSize);
        applyButton.setFont(serif12B);

        return applyButton;
    }

    /**
     * Builds the cancel button. Sets it internally as well return the just-built button.
     *
     * @return  DOCUMENT ME!
     */
    protected JButton buildCancelButton() {
        cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(this);

        // cancelButton.setToolTipText("Cancel action.");
        cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
        cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
        cancelButton.setFont(serif12B);

        return cancelButton;
    }

    /**
     * Builds the close button. Sets it internally as well return the just-built button.
     *
     * @return  DOCUMENT ME!
     */
    protected JButton buildCloseButton() {
        closeButton = new JButton("Close");
        closeButton.addActionListener(this);
        closeButton.setToolTipText("Close dialog.");
        closeButton.setMinimumSize(MipavUtil.defaultButtonSize);
        closeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        closeButton.setFont(serif12B);

        return closeButton;
    }

    /**
     * Builds the help button. Sets it internally as well
     *
     * @return  helpButton the help button
     */
    protected JButton buildHelpButton() {
        helpButton = new JButton("Help");
        helpButton.addActionListener(this);
        helpButton.setMinimumSize(MipavUtil.defaultButtonSize);
        helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
        helpButton.setFont(serif12B);

        return helpButton;
    }

    /**
     * Builds the OK button. Sets it internally as well return the just-built button.
     *
     * @return  DOCUMENT ME!
     */
    protected JButton buildOKButton() {
        OKButton = new JButton("OK");
        OKButton.addActionListener(this);

        // OKButton.setToolTipText("Accept values and perform action.");
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(serif12B);

        return OKButton;
    }

    /**
     * Builds a titled border with the given title, an etched border, and the
     * proper font and color.  Changed to public static member so that it can
     * be used for other JPanels not inherited from this base class.
     * @param   title  Title of the border
     *
     * @return  The titled border.
     */
    public static TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                                Color.black);
    }


    /**
     * Calls finalize().
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }


    /**
     * Tests that the entered parameter is in range.
     *
     * @param   str       The value entered by the user.
     * @param   minValue  The minimum value this variable may be set to.
     * @param   maxValue  The maximum value this variable may be set to.
     *
     * @return  <code>true</code> if parameters passed range test, <code>false</code> if failed.
     */
    protected boolean testParameter(String str, double minValue, double maxValue) {
        double tmp;

        try {
            tmp = Double.valueOf(str).doubleValue();

            if ((tmp > maxValue) || (tmp < minValue)) {
                MipavUtil.displayError("Value is out of range: " + String.valueOf(minValue) + " , " +
                                       String.valueOf(maxValue));

                return false;
            } else {
                return true;
            }
        } catch (NumberFormatException error) {
            MipavUtil.displayError("Must enter numeric value");

            return false;
        }
    }
}
