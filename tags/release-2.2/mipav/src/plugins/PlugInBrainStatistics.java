import gov.nih.mipav.plugins.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;

import java.awt.*;


/**
 * This is a plugin that calculates statistics about a brain image which has been classified into
 * csf/gray matter/white matter/background and prints the statistics to the data output window.
 * @author Evan McCreedy
 * @version 1.0
 */
public class PlugInBrainStatistics
    implements PlugInAlgorithm {

    /**
     *   Defines body of run method, which was declared in the interface.
     *
     *   @param  UI              User Interface
     *   @param  parentFrame     parent frame
     *   @param  image           current ModelImage - this is an image already
     *                           loaded into MIPAV. Can be null.
     *
     *   @see    ViewUserInterface
     *   @see    ModelImage
     *   @see    ViewJFrameImage
     */
    public void run( ViewUserInterface UI, Frame parentFrame, ModelImage image ) {

        if ( parentFrame instanceof ViewJFrameImage ) {
            new PlugInDialogBrainStatistics( parentFrame, image );
        } else {
            MipavUtil.displayError( "PlugIn CT_MD only runs on an image frame." );
        }
    }
}
