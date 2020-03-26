import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;

/**
 * Plugin for William Yutzy (NCTVL Lab) that estimates the Foci and Nuclei (F/N) in an image.
 * 
 * @author joshim2
 */
public class PlugInEstimateFociNuclei implements PlugInAlgorithm {
	
//	~ Methods --------------------------------------------------------------------------------------------------------

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
            new PlugInDialogEstimateFociNuclei(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugIn Estimate Foci Nuclei only runs on an image frame.");
        }
    }
    
}

