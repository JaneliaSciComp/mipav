/**
 * Start of the application - Medical Image Processing, Analysis & Visualization(MIPAV).
 *
 * This application allows users to perform quantitative analysis of
 * multi-modality 1D/2D/3D/4D/5D image datasets to assist medical research.
 *
 * @author Matthew J. McAuliffe
 */


import gov.nih.mipav.view.*;


/**
 * The class which starts up the Mipav application. Also passes along the command line arguments to the UI.
 */
public class MipavMain {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Passes control to the main GUI in the Model, View and Controller arch.
     *
     * @param  args  array of command line arguments input as strings
     */
    public static void main(String[] args) {
        System.setProperty("sun.awt.noerasebackground", "true");

        ViewUserInterface ui = ViewUserInterface.create();
        ui.parseArguments(args, 0); //begin processing command line arguments


        if (ui.isAppFrameVisible() && Preferences.is(Preferences.PREF_SHOW_SPLASH)) {
            ui.showSplashGraphics();
        }

        ui.setVisible(ui.isAppFrameVisible());
    }
}
