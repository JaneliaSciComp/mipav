import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * PlugIn to take the absolute value of the image.
 *
 * @see  PlugInAlgorithm
 */

// This is a Algorithm type of PlugIn, and therefore must implement PlugInAlgorithm
// Implementing the PlugInAlgorithm requires this class to implement the run method
// with the correct parameters
public class PlugInPipeline implements PlugInAlgorithm {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * defines body of run method, which was declared in the interface. This sample run method creates a new image in a
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

        System.err.println("running OAI thigh segmentation");

        if (parentFrame instanceof ViewJFrameImage) {
            new PlugInDialogPipeline(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugIn Wrap Fix only runs on an image frame.");
        }
    }


    /**
     * DOCUMENT ME!
     *
     * @param  UI           DOCUMENT ME!
     * @param  parentFrame  DOCUMENT ME!
     * @param  imageA       DOCUMENT ME!
     * @param  imageB       DOCUMENT ME!
     */
    public void run(ViewUserInterface UI, Frame parentFrame, ModelImage imageA, ModelImage imageB) {

        if (parentFrame instanceof ViewJFrameImage) {
            new PlugInDialogPipeline(parentFrame, imageA);
        } else {
            MipavUtil.displayError("PlugIn Wrap Fix only runs on an image frame.");
        }
    }
}
