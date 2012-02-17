import java.util.Vector;

import gov.nih.mipav.plugins.BundledPlugInInfo;
import gov.nih.mipav.plugins.PlugInBundle;
import gov.nih.mipav.plugins.PlugInGeneric;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

/**
 * This is the bundle that represents the TRE package.  The accompanying files are stored in subfolders (or in
 * a jar, or as fully qualified names, MIPAV has no preference).  TRET1, TRET2, and any other TRE plugins
 * should be enumerated here and placed in the  
 * 
 * @author senseneyj
 *
 */
public class TreBundle extends PlugInBundle {

    //a way for storing PlugInBundles that works well for large libraries
    private static Vector<BundledPlugInInfo> trePlugIns; 
    
    //a way for storing PlugInBundles that will be accessed frequently
    //private static Class<BundledPlugInInfo> treT1;
    //private static Class<BundledPlugInInfo> treT2;
    
    static {
        
        trePlugIns = new Vector<BundledPlugInInfo>();
        
        final String[] category = new String[1];
        category[0] = "Tre";
        Vector<BundledPlugInInfo> v = new Vector<BundledPlugInInfo>();
        
        v.add(new BundledPlugInInfo() {

            public String[] getCategory() {
                return category;
            }

            public String getName() {
                return "TRET1_MIPAV";
            }
        });
        
        v.add(new BundledPlugInInfo() {

            public String[] getCategory() {
                return category;
            }

            public String getName() {
                return "TRET2_MIPAV";
            }
        });
    }
    
    /**
     * Provides a list of plugins that the TRE package includes.
     * 
     * @return A list of plugins that are bundled by this class.
     */
    public Vector<BundledPlugInInfo> getBundledPlugIns() {
        return trePlugIns;
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
            Object thePlugIn = Class.forName("tre.PlugIn"+trePlugIns.get(pluginIndex)).newInstance();
            if (thePlugIn instanceof PlugInGeneric) {
                ((PlugInGeneric) thePlugIn).run();
            } else {
                MipavUtil.displayError("Plug-in " + plugInName
                        + " is not a generic plugin.");
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
