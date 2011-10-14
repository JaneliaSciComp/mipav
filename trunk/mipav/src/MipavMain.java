/**
 * Start of the application - Medical Image Processing, Analysis & Visualization(MIPAV).
 *
 * This application allows users to perform quantitative analysis of
 * multi-modality 1D/2D/3D/4D/5D image datasets to assist medical research.
 *
 * @author Matthew J. McAuliffe
 */

import java.io.File;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.CommandLineParser.StaticArgument;


/**
 * The class which starts up the Mipav application. Also passes along the command line arguments to the UI.
 */
public class MipavMain {//implements CommandLineParser { //TODO: Test effectiveness in nightly build

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Passes control to the main GUI in the Model, View and Controller arch.
     *
     * @param  args  array of command line arguments input as strings
     */
    public static void main(String[] args) {
        System.setProperty("sun.awt.noerasebackground", "true");

        //MipavMain mipav = new MipavMain();
        int initArg = MipavMain.parseArguments(args, 0); //process static command line arguments
        
        ViewUserInterface ui = ViewUserInterface.create();
        ui.parseArguments(args, initArg); //process command line arguments that require mipav objects


        if (ui.isAppFrameVisible() && Preferences.is(Preferences.PREF_SHOW_SPLASH)) {
            ui.showSplashGraphics();
        }

        ui.setVisible(ui.isAppFrameVisible());
    }

    /**
     * Required by the CommandLineParser interface. Processes MIPAV command line arguments that DO NOT require
     * MIPAV to have already been initialized. In cases like the plugins directory, it is specifically
     * required that MIPAV has not been initialized yet. Returns the next argument to be processed (finished 
     * if returns args.length)
     */
    public static int parseArguments(String[] args, int initArg) {
        int i = 0;
        String arg;
        boolean prefDirCommandDone = false;
        
        ViewUserInterface.setProvidedUserDefaultDir(false);

        ViewUserInterface.setUserDefaultDir("");
        
        if (args.length == 0) {
            return args.length;
        }
        
parse:  while (i < args.length) {
            arg = args[i];

            if (arg.startsWith("-")) {
                
              //parse commands which do not require an initialized mipav
                StaticArgument c = StaticArgument.getArgument(arg);
                if(c == null) {
                    i++;
                    continue parse;
                }
                
                switch(c) {
                
                case Help:
                    ViewUserInterface.printUsageAndExit();
                    break;
                
                case InputDir:
                    ViewUserInterface.setProvidedUserDefaultDir(true);
                    ViewUserInterface.setUserDefaultDir(args[ ++i]);
                    
                    if (ViewUserInterface.getUserDefaultDir() == null || ViewUserInterface.getUserDefaultDir().trim().equals("")) {
                        Preferences.debug("In argument -"+c+", "+"directory is null");
                        ViewUserInterface.printUsageAndExit(c);
                    } else {
                        // check that there is a trailng slash at the end of the defaultDir...if not, add one
                        if ( ! (ViewUserInterface.getUserDefaultDir().charAt(ViewUserInterface.getUserDefaultDir().length() - 1) == File.separatorChar)) {
                            ViewUserInterface.setUserDefaultDir(ViewUserInterface.getUserDefaultDir() + File.separator);
                        }
                        // now check if this is a valid path
                        final File checkDefaultDir = new File(ViewUserInterface.getUserDefaultDir());
                        if ( !checkDefaultDir.exists()) {
                            Preferences.debug("In argument -"+c+", "+ViewUserInterface.getUserDefaultDir()+" does not exist");
                            ViewUserInterface.printUsageAndExit(c);
                        }
                    }
                    break;
                    
                case OutputDir:
                    ViewUserInterface.setProvidedOutputDir(true);
                    ViewUserInterface.setOutputDir(args[ ++i]);            
                    
                    if (ViewUserInterface.getOutputDir() == null || ViewUserInterface.getOutputDir().trim().equals("")) {
                        ViewUserInterface.setProvidedOutputDir(false);
                        ViewUserInterface.printUsageAndExit(c);
                    } else {
                        // check that there is a trailng slash at the end of the defaultDir...if not, add one
                        if ( ! (ViewUserInterface.getOutputDir().charAt(ViewUserInterface.getOutputDir().length() - 1) == File.separatorChar)) {
                            ViewUserInterface.setOutputDir(ViewUserInterface.getOutputDir() + File.separator);
                        }
                        // now check if this is a valid path
                        final File checkOutputDir = new File(ViewUserInterface.getOutputDir());
                        if ( !checkOutputDir.exists()) {
                            ViewUserInterface.setProvidedOutputDir(false);
                            Preferences.debug("In argument -"+c+", "+ViewUserInterface.getOutputDir()+" does not exist");
                            ViewUserInterface.printUsageAndExit();
                        }
                    }
                    break;
                
                
                case PluginDir:
                    String secPluginsDir = args[ ++i];
                    File f = new File(secPluginsDir);
                    if(f.exists() && f.isDirectory() && f.canRead()) {
                        ViewUserInterface.setSecondaryPluginsDir(f);
                    }else {
                        Preferences.debug("In argument -"+c+", "+secPluginsDir+" is not a valid readable directory", Preferences.DEBUG_MINOR);
                        ViewUserInterface.printUsageAndExit(c);
                    }
                    break;
                
                case HomeDir:
                    Preferences.debug("HomeDir command is not currently usable, is only System.getProperty(\"user.home\")");
                    break;
                
                case PreferencesDir:
                    prefDirCommandDone = true;
                    String prefDir = args[++i];
                    f = new File(prefDir);
                    if(f.exists() && f.isDirectory() && f.canRead() && f.canWrite()) {
                        Preferences.setPreferencesFileDirectory(prefDir);
                    } else {
                        Preferences.debug("In argument -"+c+", "+prefDir+" is not a writable existing directory.", Preferences.DEBUG_MINOR);
                        ViewUserInterface.printUsageAndExit(c);
                    }
                    break;
                    
                case PreferencesName:
                    if(!prefDirCommandDone) {
                        checkPrefDirCommand(args, initArg);
                    }
                    String prefName = args[++i];
                    f = new File(prefName);
                    if(!f.exists() || (f.canRead() && f.canWrite())) {
                        Preferences.setPreferencesFileName(prefName);
                    } else {
                        Preferences.debug("In argument -"+c+", "+prefName+" is not in a readable and/or writable location.", Preferences.DEBUG_MINOR);
                        ViewUserInterface.printUsageAndExit(c);
                    }
                    break;
                }
            }
            i++;
        }
        return 0;
    }

    /**
     * If the preferences name command is about to be performed before an existing preferences directory command,
     * this guarantees that the directory command will be executed first.
     */
    private static void checkPrefDirCommand(String[] args, int initArg) {
        for(int i=0; i<args.length; i++) {
            if(StaticArgument.getArgument(args[i], true) == StaticArgument.PreferencesDir) {
                String[] argSub = new String[]{args[i], args[i+1]};
                parseArguments(argSub, 0);
            }
        }
    }

}
