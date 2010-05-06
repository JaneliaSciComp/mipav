import java.util.Vector;

import gov.nih.mipav.plugins.BundledPlugInInfo;
import gov.nih.mipav.plugins.PlugInBundle;
import gov.nih.mipav.plugins.PlugInGeneric;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

/**
 * This is the bundle that represents the DESPOT package.  The accompanying files are stored in subfolders (or in
 * a jar, or as fully qualified names, MIPAV has no preference).  DESPOT1, DESPOT2, and any other DESPOT plugins
 * should be enumerated here and placed in the  
 * 
 * @author senseneyj
 *
 */
public class DespotBundle extends PlugInBundle {

    //a way for storing PlugInBundles that works well for large libraries
    private static Vector<BundledPlugInInfo> despotPlugIns; 
    
    //a way for storing PlugInBundles that will be accessed frequently
    private static Class<BundledPlugInInfo> despot1;
    private static Class<BundledPlugInInfo> despot2;
    
    static {
        
        despotPlugIns = new Vector<BundledPlugInInfo>();
        
        final String[] category = new String[1];
        category[0] = "Despot";
        Vector<BundledPlugInInfo> v = new Vector<BundledPlugInInfo>();
        
        v.add(new BundledPlugInInfo() {

            public String[] getCategory() {
                return category;
            }

            public String getName() {
                return "DESPOT1_MIPAV";
            }
        });
        
        v.add(new BundledPlugInInfo() {

            public String[] getCategory() {
                return category;
            }

            public String getName() {
                return "DESPOT2_MIPAV";
            }
        });
    }
    
    /**
     * Provides a list of plugins that the DESPOT package includes.
     * 
     * @return A list of plugins that are bundled by this class.
     */
    public Vector<BundledPlugInInfo> getBundledPlugIns() {
        return despotPlugIns;
    }
    
    /**
     * Runs the plugin whose name is given by the location in the vector returned by <code>getBundledPlugIns()</code>.
     * Some not-large bundles could already store these classes when created by getBundledPlugIns().
     * 
     * @param pluginIndex the index of the vector returned by <code>getBundledPlugIns()</code>
     */
    public void run(int pluginIndex) {
        
        String plugInName = new String();
        try{
            Object thePlugIn = Class.forName("despot.PlugIn"+despotPlugIns.get(pluginIndex)).newInstance();
            if (thePlugIn instanceof PlugInGeneric) {
                ((PlugInGeneric) thePlugIn).run();
            } else {
                MipavUtil.displayError("Plug-in " + plugInName
                        + " claims to be an generic PlugIn, but does not implement PlugInGeneric.");
            }
        } catch (final UnsupportedClassVersionError ucve) {
            Preferences
                    .debug(
                            "Unable to load plugin: "
                                    + plugInName
                                    + " -- The plugin is probably compiled for an older version of Java than MIPAV currently supports.\n",
                            Preferences.DEBUG_MINOR);
            ucve.printStackTrace();
        } catch (final ClassNotFoundException e) {
            MipavUtil.displayError("PlugIn not found: " + plugInName);
        } catch (final InstantiationException e) {
            MipavUtil.displayError("Unable to load plugin (ins)");
        } catch (final IllegalAccessException e) {
            MipavUtil.displayError("Unable to load plugin (acc)");
        }
    }
}
