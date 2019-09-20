package gov.nih.mipav.plugins;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.*;
import java.io.FileNotFoundException;

import javax.swing.JDialog;


/**
 * This is an abstract class which should be subclassed by Generic plug-in dialogs which want to support being run with
 * the -p command line switch without the rest of MIPAV (using the -hide option). If the subclass overrides
 * windowClosing(), it should call super.windowClosing() in the first line of the method. The subclass's frame should
 * also call windowClosing(null) directly when a cancel or close button is pressed.
 */
public abstract class JDialogStandaloneScriptablePlugin extends JDialogScriptableBase implements WindowListener {
    /**
     * Passthrough to JDialogScriptableBase constructor.
     * 
     * @see JDialogScriptableBase
     */
    public JDialogStandaloneScriptablePlugin() {
        super();

        setWindowSettings();
    }

    /**
     * Passthrough to JDialogScriptableBase constructor.
     * 
     * @param modal Whether the dialog is modal.
     * 
     * @see JDialogScriptableBase
     */
    public JDialogStandaloneScriptablePlugin(boolean modal) {
        super(modal);

        setWindowSettings();
    }

    /**
     * Passthrough to JDialogScriptableBase constructor.
     * 
     * @param parent The parent frame.
     * @param modal Whether the dialog is modal.
     * 
     * @see JDialogScriptableBase
     */
    public JDialogStandaloneScriptablePlugin(Frame parent, boolean modal) {
        super(parent, modal);

        setWindowSettings();
    }

    /**
     * Passthrough to JDialogScriptableBase constructor.
     * 
     * @param parent The parent dialog.
     * @param modal Whether this dialog is modal.
     * 
     * @see JDialogScriptableBase
     */
    public JDialogStandaloneScriptablePlugin(Dialog parent, boolean modal) {
        super(parent, modal);

        setWindowSettings();
    }

    /**
     * Confirms if the user really wants to exit, then closes the application (if running without the rest of the MIPAV
     * GUI).
     * 
     * @param event Event that triggered this function.
     */
    public void windowClosing(WindowEvent event) {
        cancelFlag = true;

        if (isExitRequired()) {
            ViewUserInterface.getReference().windowClosing(event);
        }
    }

    /**
     * Returns whether the way that the plug-in is being run requires us to exit MIPAV when the window closes.
     * 
     * @return True if we should exit the program when the window is closed.
     */
    protected static final boolean isExitRequired() {
        return ViewUserInterface.getReference() != null && !ViewUserInterface.getReference().isAppFrameVisible()
                && ViewUserInterface.getReference().isPlugInFrameVisible();
    }

    /**
     * Sets the necessary plug-in window setting to get it to close correctly and have the correct icon.
     * 
     * @param window The window to set up.
     */
    public void setWindowSettings() {
        if (isExitRequired()) {
            setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
        } else {
            setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        }

        try {
            // for 1.5 compatibility
            ((Frame) getOwner()).setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));

            // no setIconImage() for JDialog in Java 1.5 API
            // setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }
    }

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    public void windowActivated(WindowEvent event) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    public void windowDeactivated(WindowEvent event) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    public void windowOpened(WindowEvent event) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    public void windowClosed(WindowEvent event) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    public void windowIconified(WindowEvent event) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    public void windowDeiconified(WindowEvent event) {}
}
