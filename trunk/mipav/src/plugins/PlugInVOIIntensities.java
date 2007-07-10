import gov.nih.mipav.plugins.*;     //needed to load PlugInAlgorithm / PlugInView / PlugInFile interface
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;

import java.awt.*;

/**
*   This is another example plugin.  It runs a wrap around fix
*   on an image, by using it's own plugin implementation of
*   the median filter (and dialog).
*
*   @see PlugInAlgorithm
*/

// This is a Algorithm type of PlugIn, and therefore must implement PlugInAlgorithm
// Implementing the PlugInAlgorithm requires this class to implement the run method
// with the correct parameters
public class PlugInVOIIntensities implements PlugInAlgorithm {

    /**
    *   defines body of run method, which was declared in the interface.
    *   This sample run method creates a new image in a new frame and
    *   calls methods in PlugInSampleDrawing to add a few VOIs.
    *
    *   @param  UI              User Interface
    *   @param  parentFrame     parent frame
    *   @param  image           current ModelImage - this is an image already
    *                           loaded into MIPAV. Can be null.
    *
    *   @see    ViewUserInterface
    *   @see    ModelImage
    *   @see    ViewJFrameImage
    */
    public void run(ViewUserInterface UI, Frame parentFrame, ModelImage image) {

        if (parentFrame instanceof ViewJFrameImage)
            new PlugInDialogVOIIntensities (parentFrame, image);

        else
            MipavUtil.displayError ("PlugIn Wrap Fix only runs on an image frame.");
	}


	public void run(ViewUserInterface UI, Frame parentFrame, ModelImage imageA, ModelImage imageB) {

        if (parentFrame instanceof ViewJFrameImage)
            new PlugInDialogVOIIntensities (parentFrame, imageA);

        else
            MipavUtil.displayError ("PlugIn Wrap Fix only runs on an image frame.");
	}
}
