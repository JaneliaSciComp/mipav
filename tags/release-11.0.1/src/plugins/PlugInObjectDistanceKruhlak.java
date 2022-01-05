import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * This is simple plugin that finds the distances between 2 different color centers of mass weighted by fluorescence and
 * thresholded within a 2D or 3D voi. Multiple VOIs can be handled. 2D or 3D color images are allowed. Only color values
 * >= threshold will be counted toward the center of mass. The thresholds are specified for each color by the user in
 * the dialog box. Be sure to hit the new VOI button for each new VOI. Note that a single VOI can contain multiple
 * contours so if mulitple curves are used without hitting the new VOI button, then the interiors of all these curves
 * will belong to the same VOI.
 *
 * @see  PlugInAlgorithm
 */
public class PlugInObjectDistanceKruhlak implements PlugInAlgorithm {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. This sample run method creates a new image in a
     * new frame and calls methods in PlugInSampleDrawing to add a few VOIs.
     *
     * @param  parentFrame  parent frame
     * @param  image        current ModelImage - this is an image already loaded into MIPAV. Can be null.
     *
     * @see    ModelImage
     * @see    ViewJFrameImage
     */
    public void run(Frame parentFrame, ModelImage image) {

        if (parentFrame instanceof ViewJFrameImage) {
            new PlugInDialogObjectDistanceKruhlak(parentFrame, image);
        } else {
            MipavUtil.displayError("PlugIn ObjectDistanceKruhlak only runs on an image frame.");
        }
    }
}
