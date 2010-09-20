import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * This is simple plugin that finds the distances from the center of geometry of a cell to 1 or 2 green regions and
 * the full distance from the center thru the green region to the blue cell boundary
 *
 * @see  PlugInAlgorithm
 */
public class PlugInCenterDistance2 implements PlugInAlgorithm {

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
            new PlugInDialogCenterDistance2(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugIn CenterDistance2 only runs on an image frame.");
        }
    }
}
