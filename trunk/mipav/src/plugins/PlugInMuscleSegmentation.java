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
        	System.out.println("Directory: "+((ViewJFrameImage)parentFrame).getImageA().getImageDirectory());
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
}
