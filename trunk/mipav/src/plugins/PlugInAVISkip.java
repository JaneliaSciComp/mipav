import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;

import java.io.File;
import java.io.IOException;

import java.util.Vector;


/**
 * Converts cheshire overlays in the given file to VOIs.
 *
 * @see  PlugInAlgorithm
 */

// This is a Generic type of PlugIn which does not require a source image to run.
public class PlugInAVISkip implements PlugInGeneric {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    //~ Instance fields ------------------------------------------------------------------------------------------------


    /** Dialog for this plugin. */
    private PlugInDialogAVISkip aviSkipDialog;
    
    private String fileName;
    
    private String directory;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. Run method converts cheshire overlays in the
     * given file to VOIs.
     *
     * @see  ViewUserInterface
     * @see  ModelImage
     * @see  ViewJFrameImage
     */
    public void run() {
        aviSkipDialog = new PlugInDialogAVISkip(false, this);
        

        

    }

    /**
     * Runs the plugin.
     */

    public void runPlugin() {

        if (aviSkipDialog.isSuccessfulExit()) {
            fileName = aviSkipDialog.getFileName();
            directory = aviSkipDialog.getDirectory();
            System.out.println("directory = " + directory);
            System.out.println("fileName = " + fileName);
            
        } else {
            // Do nothing since individual error is already displayed.
        }
    }
}
