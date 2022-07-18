import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface


/**
	 * This is simple plugin that applies the same transfer function to multiple images
	 * 
	 * @see  PlugInGeneric
	 */


public class PlugInApplyTransferFunction implements PlugInGeneric{

    /**
     * Defines body of run method, which was declared in the interface.
     */
    public void run() {
        new PlugInDialogApplyTransferFunction(false);
    }

}
