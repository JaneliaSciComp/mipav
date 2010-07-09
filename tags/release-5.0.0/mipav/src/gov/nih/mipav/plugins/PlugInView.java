package gov.nih.mipav.plugins;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * DOCUMENT ME!
 */
public interface PlugInView extends PlugIn {
	
	// ~ Static fields/initializers ------------------------------------------------------------------
	
	public static final String[] CATEGORY = {"View"};

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * run.
     *
     * @param       parentFrame  frame that displays the MIPAV image. Can be used as a parent frame when building
     *                           dialogs.
     * @param       image        model of the MIPAV image.
     *
     * @see         ModelImage
     * @see         ViewJFrameImage
     */
    void run(Frame parentFrame, ModelImage image);
}
