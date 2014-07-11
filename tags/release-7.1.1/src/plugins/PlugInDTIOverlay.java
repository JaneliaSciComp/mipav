import java.awt.Frame;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.PlugInAlgorithm;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;


public class PlugInDTIOverlay implements PlugInAlgorithm {

	/**
     * Entry point to DTI Plugin
     *
     * @param  parentFrame  parent frame
     * @param  image        current ModelImage
     *
     * References: Developed in concert with Bennett Landman from Johns Hopkins University
     */
    public void run(Frame frame, ModelImage image) {

        if (frame instanceof ViewJFrameImage) {
            new PlugInDialogDTIOverlay((ViewJFrameImage)frame, image);
        } else {
            MipavUtil.displayError("PlugIn DTIOverlay only runs on an image frame.");
        }
    }

}
