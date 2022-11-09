import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * This is simple plugin that finds the 2D and 3D major dimaeters of the ellipsoids that best fit VOIs and stores the
 * VOIs in the same directory as the .txt statistics file.
 *
 * @see  PlugInAlgorithm
 */
public class PlugInFibroid implements PlugInAlgorithm {

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
            new PlugInDialogFibroid(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugIn Fibroid only runs on an image frame.");
        }
    }
}
