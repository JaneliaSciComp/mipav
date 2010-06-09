import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.io.File;
import javax.swing.JFileChooser;


/**
 * This plugin builds a simple user-interface and modifies the main mipav interface to provide custom VOI functionality for a plugin.
 * Although this plugin does not have an associated algorithm, its dialog is the PlugInDialogImageVOIDisplay.java
 * 
 * @see  PlugInGeneric
 */

// This is a Generic type of PlugIn which does not require a source image to run.
public class PlugInImageVOIDisplay implements PlugInGeneric {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
    
    //~ Instance fields ------------------------------------------------------------------------------------------------


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. Run method starts custom-made dialog located in
     * PlugInDialogImageVOIDisplay.java
     *
     * @see  ViewUserInterface
     * @see  ModelImage
     * @see  ViewJFrameImage
     */
    public void run() {

    	//ViewUserInterface.getReference().setForceInPlace(true);
    	ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
        JFileChooser chooser = fileChooser.getFileChooser();
        chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        chooser.setMultiSelectionEnabled(false);

        int returnVal = chooser.showOpenDialog(null);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            
            File imageFile = chooser.getSelectedFile();
            ViewUserInterface.getReference().setDefaultDirectory(imageFile.getParent());
          
            FileIO fileIO = new FileIO();
           
             new PlugInDialogImageVOIDisplay(fileIO.readImage(imageFile.getAbsolutePath()));
        }
    }
}
