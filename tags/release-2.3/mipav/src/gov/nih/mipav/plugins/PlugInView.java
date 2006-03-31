package gov.nih.mipav.plugins;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;


public interface PlugInView extends PlugIn {
    
    /**
    *   run 
    *   @param UI           MIPAV main user interface.
    *   @param parentFrame  frame that displays the MIPAV image.
    *                       Can be used as a parent frame when building
    *                       dialogs.
    *   @param image        model of the MIPAV image. 
    *   @see   ModelImage
    *   @see   ViewJFrameImage
    *
    */
    public void run(ViewUserInterface UI, Frame parentFrame, ModelImage image);
}