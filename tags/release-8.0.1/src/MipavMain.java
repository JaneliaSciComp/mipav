/**
 * Start of the application - Medical Image Processing, Analysis & Visualization(MIPAV).
 * 
 * This application allows users to perform quantitative analysis of multi-modality 1D/2D/3D/4D/5D image datasets to
 * assist medical research.
 * 
 * @author Matthew J. McAuliffe
 */

import gov.nih.mipav.util.MipavExceptionHandler;

import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

import javax.swing.UIManager;


/**
 * The class which starts up the Mipav application. Also passes along the command line arguments to the UI.
 */
public class MipavMain {

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Passes control to the main GUI in the Model, View and Controller arch.
     * 
     * @param args array of command line arguments input as strings
     */
    public static void main(final String[] args) {
        System.setProperty("sun.awt.noerasebackground", "true");

        Thread.setDefaultUncaughtExceptionHandler(new MipavExceptionHandler());

        try {
            final int initArg = ViewUserInterface.parseStaticArguments(args, 0); // process static command line
                                                                                 // arguments

            final ViewUserInterface ui = ViewUserInterface.create();
            ui.parseArguments(args, initArg); // process command line arguments that require mipav objects

            if (ui.isAppFrameVisible() && gov.nih.mipav.view.Preferences.is(gov.nih.mipav.view.Preferences.PREF_SHOW_SPLASH)) {
                ui.showSplashGraphics();
            }

            String lf = null;
            try {

                lf = Preferences.getProperty(Preferences.PREF_SHOW_UI_LF);
                final String os = System.getProperty("os.name");
                if (lf != null && !lf.equals("None") && !os.toLowerCase().contains("mac")) {
                    UIManager.setLookAndFeel(lf);
                }
            } catch (final Exception e) {
                System.err.println("Unable to set requested look and feel: " + lf);
            }

            ui.setVisible(ui.isAppFrameVisible());
        } catch (final Throwable e) {
            e.printStackTrace();
        }
    }
}
