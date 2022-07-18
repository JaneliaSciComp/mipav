package gov.nih.mipav.plugins;


import java.util.Vector;


/**
 * An abstract class for a plugin which reports other available plugins bundled inside of it. These plugins are then
 * shown individually under the plugin menu and started by this plugin. Any PlugInBundle subclass should override the
 * getBundledPlugIns() method
 * 
 * @author mccreedy
 */
public abstract class PlugInBundle implements PlugIn {
    
    // ~ Static fields/initializers ------------------------------------------------------------------
    
    public static final String[] CATEGORY = {"Bundle"};
    
    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Subclasses should override this method to return the list of plugins that they encompass.
     * 
     * @return A list of plugins that are bundled by this class.
     */
    public Vector<BundledPlugInInfo> getBundledPlugIns() {
        return new Vector<BundledPlugInInfo>();
    }

    /**
     * Runs the plugin whose name is given by the location in the vector returned by <code>getBundledPlugIns()</code>.
     * 
     * @param pluginIndex the index of the vector returned by <code>getBundledPlugIns()</code>
     */
    public abstract void run(int pluginIndex);
}
