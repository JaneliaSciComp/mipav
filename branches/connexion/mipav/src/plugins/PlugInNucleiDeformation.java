import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface


/**
 * This is simple plugin that identifies nuclei and measures their deformation characteristics
 *
 * @see  PlugInGeneric
 */
public class PlugInNucleiDeformation implements PlugInGeneric {
    /**
     * Defines body of run method, which was declared in the interface.
     */
    public void run() {
        new PlugInDialogNucleiDeformation(false);
    }
}
