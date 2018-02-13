import gov.nih.mipav.plugins.PlugInGeneric;

/**
 * Plugin that parses and searches the IMFAR document
 * @author pandyan
 *
 */
public class PlugInIMFAR implements PlugInGeneric {
	/**
	 * constructor
	 */
	public PlugInIMFAR() {
		
	}
	
	/**
	 * run
	 */
	public void run() {
		new PlugInDialogIMFAR(false);

	}

}
