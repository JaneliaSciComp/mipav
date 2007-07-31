import java.awt.Frame;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.PlugInAlgorithm;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;


public class PlugInDTIOverlay implements PlugInAlgorithm {

	/**
     * Entry point to DTI Plugin
     *
     * @param  UI           User Interface
     * @param  parentFrame  parent frame
     * @param  image        current ModelImage
     *
     * References: Developed in concert with Bennett Landman from Johns Hopkins University
     */
    public void run(ViewUserInterface UI, Frame frame, ModelImage image) {

        if (frame instanceof ViewJFrameImage) {
            new PlugInDialogDTIOverlay((ViewJFrameImage)frame, image);
        } else {
            MipavUtil.displayError("PlugIn DTIOverlay only runs on an image frame.");
        }
    }

}
