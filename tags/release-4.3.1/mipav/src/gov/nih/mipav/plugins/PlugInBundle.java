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
    /**
     * Subclasses should override this method to return the list of plugins that they encompass.
     * 
     * @return A list of plugins that are bundled by this class.
     */
    public Vector<BundledPlugInInfo> getBundledPlugIns() {
        return new Vector<BundledPlugInInfo>();
    }

    public abstract void run(int pluginIndex);
}
