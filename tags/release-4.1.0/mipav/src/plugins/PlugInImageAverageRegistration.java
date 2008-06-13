import java.awt.Frame;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.PlugInAlgorithm;
import gov.nih.mipav.plugins.PlugInGeneric;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * @author pandyan
 * 
 * This is the interface for the
 * Image Average Registration plugin
 *
 */
public class PlugInImageAverageRegistration implements PlugInGeneric {
	
	/**
	 * constructor
	 */
	public PlugInImageAverageRegistration() {
		
	}
	
	/**
	 * run method
	 */
	public void run() {
		new PlugInDialogImageAverageRegistration(false);
	}

}
