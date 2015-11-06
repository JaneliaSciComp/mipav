package gov.nih.mipav.plugins;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.Dialog;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.FileNotFoundException;

import javax.swing.WindowConstants;


/**
 * This is an abstract class which should be subclassed by Generic plug-in dialogs which want to support being run with
 * the -p command line switch without the rest of MIPAV (using the -hide option). If the subclass overrides
 * windowClosing(), it should call super.windowClosing() in the first line of the method. The subclass's frame should
 * also call windowClosing(null) directly when a cancel or close button is pressed.
 */
public class JDialogStandalonePlugin extends JDialogBase implements WindowListener {
    /**
     * Passthrough to JDialogBase constructor.
     * 
     * @see JDialogBase
     */
    public JDialogStandalonePlugin() {
        super();
        setVisibleMenuBar(false);

        setWindowSettings();
    }

    /**
     * Passthrough to JDialogBase constructor.
     * 
     * @param modal Whether the dialog is modal.
     * 
     * @see JDialogBase
     */
    public JDialogStandalonePlugin(final boolean modal) {
        super(modal);
        setVisibleMenuBar(false);

        setWindowSettings();
    }

    /**
     * Passthrough to JDialogBase constructor.
     * 
     * @param parent The parent frame.
     * @param modal Whether the dialog is modal.
     * 
     * @see JDialogBase
     */
    public JDialogStandalonePlugin(final Frame parent, final boolean modal) {
        super(parent, modal);
        setVisibleMenuBar(false);

        setWindowSettings();
    }

    /**
     * Passthrough to JDialogBase constructor.
     * 
     * @param parent The parent dialog.
     * @param modal Whether this dialog is modal.
     * 
     * @see JDialogBase
     */
    public JDialogStandalonePlugin(final Dialog parent, final boolean modal) {
        super(parent, modal);
        setVisibleMenuBar(false);

        setWindowSettings();
    }

    /**
     * Confirms if the user really wants to exit, then closes the application (if running without the rest of the MIPAV
     * GUI).
     * 
     * @param event Event that triggered this function.
     */
    @Override
    public void windowClosing(final WindowEvent event) {
        cancelFlag = true;

        if (JDialogStandalonePlugin.isExitRequired()) {
            ViewUserInterface.getReference().windowClosing(event);
        }
    }

    /**
     * Returns whether the way that the plug-in is being run requires us to exit MIPAV when the window closes.
     * 
     * @return True if we should exit the program when the window is closed.
     */
    public static final boolean isExitRequired() {
        return ViewUserInterface.getReference() != null && !ViewUserInterface.getReference().isAppFrameVisible()
                && ViewUserInterface.getReference().isPlugInFrameVisible();
    }

    /**
     * Sets the necessary plug-in window setting to get it to close correctly and have the correct icon.
     * 
     * @param window The window to set up.
     */
    public void setWindowSettings() {
        if (JDialogStandalonePlugin.isExitRequired()) {
            setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        } else {
            setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        }

        try {
            // for 1.5 compatibility
            ((Frame) getOwner()).setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));

            // no setIconImage() for JDialog in Java 1.5 API
            // setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() + ">.  Check that this file is available.\n");
        }
    }

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    @Override
    public void windowActivated(final WindowEvent event) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    @Override
    public void windowDeactivated(final WindowEvent event) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    @Override
    public void windowOpened(final WindowEvent event) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    @Override
    public void windowClosed(final WindowEvent event) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    @Override
    public void windowIconified(final WindowEvent event) {}

    /**
     * Do nothing.
     * 
     * @param event the window event.
     */
    @Override
    public void windowDeiconified(final WindowEvent event) {}

    /**
     * Do super.actionPerformed(). Gives subclasses the option to not include an actionPerformed method implementation.
     * 
     * @param event The action event.
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        super.actionPerformed(event);
    }
}
