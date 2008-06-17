import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.Enumeration;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;

import javax.help.BadIDException;
import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.help.HelpSetException;
import javax.swing.JFileChooser;


/**
 * This is simple plugin that creates an image with only the kidneys from an image of the 
 * abdominal cavity.
 *
 * @see  PlugInAlgorithm
 * 
 * @author senseneyj
 */
public class PlugInMuscleSegmentation implements PlugInAlgorithm, PlugInGeneric {
	
	/**
     * Displays the Java Help dialog indexed directly to the 
     * section identified by the ID passed in.*/
    static HelpSet hs;

    /** Assists in finding correct location in helpfile */
    static HelpBroker helpBroker;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. 
     *
     * @param  parentFrame  parent frame
     * @param  image        current ModelImage
     *
     * @see    ModelImage
     * @see    ViewJFrameImage
     */
    public void run(Frame parentFrame, ModelImage image) {

        if (parentFrame instanceof ViewJFrameImage) {
        	//clones the ModelImage before passing into the plugin dialog
            new PlugInDialogMuscleSegmentation(ViewUserInterface.getReference().getMainFrame(), 
            		(ModelImage)image.clone(JDialogBase.makeImageName(image.getImageName(), "_muscle_seg")));
        } 
    }
    
    /**
     * Method for calling this from the command line (PlugInGeneric implementation)
     *   user selects a file, no changes mecessary for 3D impl.
     */
    public void run() {
//    	if run from the command line, can be self-contained
    	
    	
    	ViewUserInterface.getReference().setExitCmdLineOnError(false);
    	ViewUserInterface.getReference().setPlugInFrameVisible(!ViewUserInterface.getReference().isAppFrameVisible());
    	ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
        JFileChooser chooser = fileChooser.getFileChooser();
        chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        chooser.setMultiSelectionEnabled(false);

        int returnVal = chooser.showOpenDialog(null);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File imageFile = chooser.getSelectedFile();
            ViewUserInterface.getReference().setDefaultDirectory(imageFile.getParent());
          
            FileIO fileIO = new FileIO();
           
             new PlugInDialogMuscleSegmentation(ViewUserInterface.getReference().getMainFrame(),
            		 fileIO.readImage(imageFile.getAbsolutePath()));
        }
    }
    
    /**
     * Displays the correct section of the help file by the given ID
     *
     * @param  ID  location of helpfile to display, for this class will always begin as MS
     */
    //TODO: Remove extra help code
    public static void showHelp(String ID) {


        try {
            ClassLoader cloader = help.PlaceHolderHelp.class.getClassLoader();

            // the help.jar must be in the classpath !!!!!!!!!!
            URL hsURL = HelpSet.findHelpSet(cloader, getHelpSetInJar("MuscleSegmentationPlug-inHelp.jar"));

            // System.out.println(" URL = " + hsURL.toString());
            if (hsURL != null) {

                if ((hs == null) || (helpBroker == null)) {
                    hs = new HelpSet(cloader, hsURL);
                    helpBroker = hs.createHelpBroker();
                }

                if (ID != null) {
                    helpBroker.setCurrentID(ID);
                }

                helpBroker.setSize(new java.awt.Dimension(1000, 600));
                helpBroker.setLocation(new java.awt.Point(200, 300));

                // helpBroker.getFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                helpBroker.setDisplayed(true);
                // hs = null;
                // helpBroker = null;
            } else {
                Preferences.debug("Help file URL is " + hsURL + "\n");
                MipavUtil.displayError("Unable to find helpset.");
            }
        } catch (NullPointerException npe) {
            Preferences.debug("MIPAV Help cannot be found." + "\n", 2);
            MipavUtil.displayError("MIPAV Help cannot be found.");
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory error opening help.");
        } catch (HelpSetException error) {
            Preferences.debug("HelpSet error = " + error);
        } catch (BadIDException error) {
            MipavUtil.displayError("HelpSet ID error = " + error);
            showHelp(10000);
        }
    }
    
    /**
     * Displays the Java Help dialog indexed directly to the section identified by the ID passed in.
     *
     * @param  ID  the index ID indicating what section the Java Help dialop should display.
     */
    public static void showHelp(int ID) {
        HelpSet hs;
        HelpBroker helpBroker;

        try {
            ClassLoader cloader = help.PlaceHolderHelp.class.getClassLoader();

            // the help.jar must be in the classpath !!!!!!!!!!
            URL hsURL = HelpSet.findHelpSet(cloader, getHelpSetInJar("MuscleSegmentationPlug-inHelp.jar"));

            // System.out.println(" URL = " + hsURL.toString());
            if (hsURL != null) {
                hs = new HelpSet(cloader, hsURL);
                helpBroker = hs.createHelpBroker();

                // if( ID != null) helpBroker.setCurrentID(ID);
                // helpBroker.setCurrentID(Map.ID.create();
                helpBroker.setSize(new java.awt.Dimension(1000, 600));
                helpBroker.setLocation(new java.awt.Point(200, 300));

                // helpBroker.getFrame().setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                helpBroker.setDisplayed(true);
            } else {
                Preferences.debug("Help file URL is " + hsURL + "\n");
                MipavUtil.displayError("Unable to find helpset.");
            }
        } catch (NullPointerException npe) {
            Preferences.debug("Muscle segmentation help cannot be found." + "\n", 2);
            MipavUtil.displayError("Muscle segmentation help cannot be found.");
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory error opening help.");
        } catch (HelpSetException error) {
            Preferences.debug("HelpSet error = " + error);
        }
    }
    
    /**
     * Finds the help set file within the JAR file. The help set file should end in ".hs"
     *
     * @param   jarFile  the name of the JAR file which contain help information
     *
     * @return  the help set file name
     */
    private static String getHelpSetInJar(String jarFile) {
        String hsName = null;
        String jarFullPath = null;

        try {
            URL urlToHelpJar = help.PlaceHolderHelp.class.getResource(jarFile);

            jarFullPath = URLDecoder.decode(urlToHelpJar.getPath(), "UTF-8");
        } catch (Exception error) {
            Preferences.debug("Problems finding "+jarFile+" jar file.");

            return null;
        }

        try {
            JarFile jar = new JarFile(new File(jarFullPath));
            Enumeration entries = jar.entries();

            while (entries.hasMoreElements()) {
                ZipEntry entry = (ZipEntry) entries.nextElement();
                String entryName = entry.getName();

                // System.out.println("file name in jar = " + entryName);
                if (entryName.endsWith(".hs")) {
                    hsName = entryName;

                    break;
                }
            }
            // MipavUtil.displayError("Unable to find helpset file.hs.");
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory error opening help.");
        } catch (IOException ee) {
            Preferences.debug("Problems opening "+jarFile+" jar file.");
        }

        // System.out.println("Help set file name in jar = " + hsName);
        return hsName;
    }
}
