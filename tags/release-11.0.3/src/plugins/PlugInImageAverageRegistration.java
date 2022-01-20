import gov.nih.mipav.plugins.PlugInGeneric;

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
