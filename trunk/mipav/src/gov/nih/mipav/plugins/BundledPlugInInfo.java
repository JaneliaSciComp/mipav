package gov.nih.mipav.plugins;

/**
 * This interface binds a PlugIn to a PlugInBundle.  Every PlugInBundle contains a list of BundledPlugInInfos 
 * that may also be PlugIns.  These BundledPlugInInfos can be run by specifying an index of the PlugInBundle's 
 * <code>run(int index)</code> method.
 * 
 * @author mccreedy
 */
public interface BundledPlugInInfo {
    
    /**
     * Specifies the menu structure of a particular plug-in.  This menu structure is, 
     * by default, displayed as a sub-menu of a given PlugInBundle.
     */
    public String[] getCategory();

    /**
     * Returns the name of the plug-in.  This could possibly be used for scripting purposes.
     */
    public String getName();
}
