import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * This is simple plugin that counts the number of foci and the number of foci on strands.
 *
 * @see  PlugInAlgorithm
 */
public class PlugInFociStrandMesh implements PlugInAlgorithm {

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
            new PlugInDialogFociStrandMesh(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugIn FociStrandMesh only runs on an image frame.");
        }
    }
}
