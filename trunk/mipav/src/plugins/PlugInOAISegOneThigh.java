import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * PlugIn to segment MR images of the thighs into 
 * 	1) interstitial fat
 * 	2) subcutaneous fat 
 * 	3) muscle
 * 	4) bone
 * 	5) bone marrow
 *
 * @version  1.0, June 20, 2007
 * @author   Lydia Beasley
 */

// Main PlugIn driver for the OAI segmentation algorithm.  Overloads the run method
// to begin the processing
public class PlugInOAISegOneThigh implements PlugInAlgorithm {

	
	/**
	 * @param UI			user interface
	 * @param parentFrame	parent frame
	 * @param image			image that invoked the plugin	
	 */
    public void run(ViewUserInterface UI, Frame parentFrame, ModelImage image) {

        System.out.println("running OAI single thigh segmentation");

        if (parentFrame instanceof ViewJFrameImage) {
            new PlugInDialogOAISegOneThigh(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugIn Wrap Fix only runs on an image frame.");
        }
    }
}
