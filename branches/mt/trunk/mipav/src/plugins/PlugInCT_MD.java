import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * This is simple plugin for the University of Maryland to simple segment an image based on CT Hounsfield units.
 *
 * @see  PlugInAlgorithm
 */

// This is a Algorithm type of PlugIn, and therefore must implement PlugInAlgorithm
// Implementing the PlugInAlgorithm requires this class to implement the run method
// with the correct parameters
public class PlugInCT_MD implements PlugInAlgorithm {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. This sample run method creates a new image in a
     * new frame and calls methods in PlugInSampleDrawing to add a few VOIs.
     *
     * @param  parentFrame  parent frame
     * @param  image        current ModelImage - this is an image already loaded into MIPAV. Can be null.
     *
     * @see    ModelImage
     * @see    ViewJFrameImage
     */
    public void run(Frame parentFrame, ModelImage image) {

        if (parentFrame instanceof ViewJFrameImage) {
            new PlugInDialogCT_MD(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugIn CT_MD only runs on an image frame.");
        }
    }


}
