import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface


/**
 * This is simple plugin that reads in images and VOI segmentations of nuclei in those images and then outputs nuclei deformation statistics.
 *
 * @see  PlugInGeneric
 */
public class PlugInNucleiStatistics implements PlugInGeneric {
	public static final String[] CATEGORY = {"Nuclei blebbing"};
	
    /**
     * Defines body of run method, which was declared in the interface.
     */
    public void run() {
        new PlugInDialogNucleiStatistics(false);
    }
}
