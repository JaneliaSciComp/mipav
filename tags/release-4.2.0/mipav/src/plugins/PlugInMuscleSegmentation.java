import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.*;
import java.util.ArrayList;
import java.util.Vector;
import javax.help.HelpBroker;
import javax.help.HelpSet;


/**
 * This is simple plugin that creates an image with only the kidneys from an image of the 
 * abdominal cavity.
 *
 * @see  PlugInAlgorithm
 * 
 * @author senseneyj
 */
public class PlugInMuscleSegmentation implements PlugInAlgorithm {
	
	/**
     * Displays the Java Help dialog indexed directly to the 
     * section identified by the ID passed in.*/
    static HelpSet hs;

    /** Assists in finding correct location in helpfile */
    static HelpBroker helpBroker;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. 
     *
     * @param  parentFrame  parent frame
     * @param  image        current ModelImage
     *
     * @see    ModelImage
     * @see    ViewJFrameImage
     */
    public void run(Frame parentFrame, ModelImage image) {

        if (parentFrame instanceof ViewJFrameImage) {
        	//clones the ModelImage before passing into the plugin dialog
        	System.out.println("Directory: "+((ViewJFrameImage)parentFrame).getImageA().getImageDirectory());
            new PlugInDialogMuscleSegmentation(ViewUserInterface.getReference().getMainFrame(), 
            		(ModelImage)image.clone(JDialogBase.makeImageName(image.getImageName(), "_muscle_seg")));
        } 
    }
    
    /**
     * Method for calling this from the command line (PlugInGeneric implementation)
     *   user selects a file, refitted for 3D impl.
     */
    public void run() {
//    	if run from the command line, can be self-contained
    	ViewOpenFileUI ui = new ViewOpenFileUI(true);
    	
    	ArrayList<Vector<String>> openImagesArrayList = ui.open(false, false);
    	
    	if (openImagesArrayList != null) {
            for (int i = 0; i < openImagesArrayList.size(); i++) {
                
            	Vector<String> openImageNames = openImagesArrayList.get(i);

                // if open failed, then imageNames will be null
                if (openImageNames == null) {
                    return;
                }
            }
    	}
    	
        new PlugInDialogMuscleSegmentation(ViewUserInterface.getReference().getMainFrame(), ui.getImage());
    }
}
