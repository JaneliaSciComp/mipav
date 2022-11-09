import gov.nih.mipav.plugins.PlugInAlgorithm;

import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Frame;

public class PlugInSaveGEHeader  implements PlugInAlgorithm {

    //public static final String[] CATEGORY = {"Test", "A", "B"};

    public void run(Frame parentFrame, ModelImage image) {

        if (parentFrame instanceof ViewJFrameImage) {
            new PlugInDialogSaveGEHeader(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugInSaveGE4XHeader only runs on an image frame.");
        }
    }
}
