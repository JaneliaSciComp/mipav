import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * PlugIn to segment MR images of the thighs into:
 * <ol>
 * <li>interstitial fat</li>
 * <li>subcutaneous fat</li>
 * <li>muscle</li>
 * <li>bone</li>
 * <li>bone marrow</li>
 * </ol>
 * 
 * Main PlugIn driver for the OAI segmentation algorithm.  Overloads the run method to begin the processing
 *
 * @version  1.0, June 22, 2006
 * @author   Agatha Monzon, Paul Hemler, and Matthew J. McAuliffe, Ph.D.
 */
public class PlugInOAISeg implements PlugInAlgorithm {

	
	/**
	 * @param parentFrame	parent frame
	 * @param image			image that invoked the plugin	
	 */
    public void run(Frame parentFrame, ModelImage image) {

        System.out.println("running OAI thigh segmentation");

        if (parentFrame instanceof ViewJFrameImage) {
            new PlugInDialogOAISeg(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugIn Wrap Fix only runs on an image frame.");
        }
    }
}
