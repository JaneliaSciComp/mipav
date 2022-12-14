import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;

// This is a silly change to see if I can Commit with SVN 
/**
 * This is simple plugin computes and displays various statistics of an image
 *
 * @see  PlugInAlgorithm
 */
public class PlugInImageStatistics implements PlugInAlgorithm {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. 
     *
     * @param  parentFrame  parent frame
     * @param  image        current ModelImage - this is an image already loaded into MIPAV. Can be null.
     *
     * @see    ModelImage
     * @see    ViewJFrameImage
     */
    public void run(Frame parentFrame, ModelImage image) {

        if (parentFrame instanceof ViewJFrameImage) {
            new PlugInDialogImageStatistics(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugInNewGeneric only runs on an image frame.");
        }
    }

}
