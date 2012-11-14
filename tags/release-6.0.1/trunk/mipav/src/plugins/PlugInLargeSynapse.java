

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface



/**
 * This is simple plugin that sweeps the images with sets of 16 parallel lines to find all red, blue, green synapses.
 *
 * @see  PlugInGeneric
 */
public class PlugInLargeSynapse implements PlugInGeneric {
    
    

    //~ Methods --------------------------------------------------------------------------------------------------------

    public void run() {
        new PlugInDialogLargeSynapse(false);
    }
    
}
