import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * This is a simple plugin that measures the Standardized Uptake Values from VOIs in 3D PET images.
 *
 * @see  PlugInAlgorithm
 */

// This is an Algorithm type of PlugIn, and therefore must implement
// PlugInAlgorithm
// Implementing the PlugInAlgorithm requires this class to implement the run
// method
// with the correct parameters
public class PlugInSUV_PET implements PlugInAlgorithm {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. This sample run method creates a new image in a
     * new frame and calls methods in PlugInSampleDrawing to add a few VOIs.
     *
     * @param  UI           User Interface
     * @param  parentFrame  parent frame
     * @param  image        current ModelImage - this is an image already loaded into MIPAV. Can be null.
     *
     * @see    ViewUserInterface
     * @see    ModelImage
     * @see    ViewJFrameImage
     */
    public void run(ViewUserInterface UI, Frame parentFrame, ModelImage image) {

        if (parentFrame instanceof ViewJFrameImage) {
            new PlugInDialogSUV_PET(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugIn SUV_PET only runs on an image frame.");
        }
    }
}
