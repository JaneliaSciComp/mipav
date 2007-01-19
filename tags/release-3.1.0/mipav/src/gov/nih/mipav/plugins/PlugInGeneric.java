package gov.nih.mipav.plugins;


/**
 * An interface for generic MIPAV Plug-Ins which don't require an open image and can be run from the ViewUserInterface
 * menu.
 *
 * @author  mccreedy
 */
public interface PlugInGeneric extends PlugIn {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * The main method used to start the generic plugin.
     */
    void run();
}
