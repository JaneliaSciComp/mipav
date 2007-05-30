import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;
/**
 * @author joshim2
 * Plugin for William Yutzy (NCTVL Lab) that estimates the Foci and Nuclei (F/N) in an image.
 */

//This is a Algorithm type of PlugIn, and therefore must implement PlugInAlgorithm
//Implementing the PlugInAlgorithm requires this class to implement the run method
//with the correct parameters
public class PlugInEstimateFociNuclei implements PlugInAlgorithm {
	
//	~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface.
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
            new PlugInDialogEstimateFociNuclei(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugIn Estimate Foci Nuclei only runs on an image frame.");
        }
    }
    
}

