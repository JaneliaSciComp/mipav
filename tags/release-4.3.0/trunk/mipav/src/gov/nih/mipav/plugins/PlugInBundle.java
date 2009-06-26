package gov.nih.mipav.plugins;


import java.util.Vector;


/**
 * An interface for a plugin which reports other available plugins bundled inside of it. These plugins are then shown
 * individually under the plugin menu and started by this plugin.
 * 
 * @author mccreedy
 */
public interface PlugInBundle extends PlugIn {
    public static final String[] CATEGORY = {"Plugin bundle"};

    Vector<BundledPlugInInfo> getBundledPlugIns();

    void run(int pluginIndex);
}
