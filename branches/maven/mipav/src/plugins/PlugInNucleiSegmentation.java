import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface


/**
 * This is simple plugin that identifies nuclei, allows the user to review the segmentation, and saves the nuclei VOIs.
 *
 * @see  PlugInGeneric
 */
public class PlugInNucleiSegmentation implements PlugInGeneric {
	public static final String[] CATEGORY = {"Nuclei blebbing"};
	
    /**
     * Defines body of run method, which was declared in the interface.
     */
    public void run() {
        new PlugInDialogNucleiSegmentation(false);
    }
}
