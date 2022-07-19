package gov.nih.mipav.plugins;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * This is the MIPAV plugin interface for Algorithms. New algorithms are added by implementing the run method of this
 * interfae in a class.
 *
 * @author  mccreedy
 */
public interface PlugInAlgorithm extends PlugIn {
	
	// ~ Static fields/initializers ------------------------------------------------------------------
	
	public static final String[] CATEGORY = {"Algorithm"};

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Run the plugin algorithm.
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
