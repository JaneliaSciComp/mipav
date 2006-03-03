import gov.nih.mipav.plugins.*; //needed to load PlugInAlgorithm / PlugInView / PlugInFile interface
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;

import java.awt.*;

/**
 * This is simple plugin that finds the distances from the center of mass of
 * cell regions to the center of mass of the cell and the distances from the
 * center of mass of cell regions to the edge of the cell
 * 
 * @see PlugInAlgorithm
 */

// This is a Algorithm type of PlugIn, and therefore must implement
// PlugInAlgorithm
// Implementing the PlugInAlgorithm requires this class to implement the run
// method
// with the correct parameters
public class PlugInRegionDistance implements PlugInAlgorithm {

    /**
     * Defines body of run method, which was declared in the interface. This
     * sample run method creates a new image in a new frame and calls methods in
     * PlugInSampleDrawing to add a few VOIs.
     * 
     * @param UI
     *            User Interface
     * @param parentFrame
     *            parent frame
     * @param image
     *            current ModelImage - this is an image already loaded into
     *            MIPAV. Can be null.
     * 
     * @see ViewUserInterface
     * @see ModelImage
     * @see ViewJFrameImage
     */
    public void run(ViewUserInterface UI, Frame parentFrame, ModelImage image) {

        if (parentFrame instanceof ViewJFrameImage)
            new PlugInDialogRegionDistance(parentFrame, image);

        else
            MipavUtil.displayError("PlugIn RegionDistance only runs on an image frame.");
    }
}
