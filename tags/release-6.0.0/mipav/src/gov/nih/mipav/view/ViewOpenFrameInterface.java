package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;


/**
 * Simple interface to allow JDialogs to open up a new frame of the same type as was used to create the dialog
 * This allows stand-alone plugins that extend ViewJFrameImage to instantiate a new frame of the same type.
 * @author linkb
 *
 */
public interface ViewOpenFrameInterface {

    //~ Methods --------------------------------------------------------------------------------------------------------

    public ViewJFrameImage openFrame(ModelImage image);

}
