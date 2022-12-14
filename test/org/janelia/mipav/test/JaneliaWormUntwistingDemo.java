package org.janelia.mipav.test;

import javax.swing.UIManager;

import org.janelia.mipav.plugins.worm.untwisting.PlugInDialogVolumeRenderDualJanelia;

import gov.nih.mipav.util.MipavExceptionHandler;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

public class JaneliaWormUntwistingDemo {
	public static void runDemo() {
		PlugInDialogVolumeRenderDualJanelia plugin = new PlugInDialogVolumeRenderDualJanelia();
		plugin.setDemoValues();
	}
	public static void main(String[] args) {
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

		
		runDemo();
	}
}
