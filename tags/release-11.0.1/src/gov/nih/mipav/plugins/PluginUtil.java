package gov.nih.mipav.plugins;

import gov.nih.mipav.view.MipavUtil;

import java.awt.Container;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Vector;

import javax.swing.JMenuItem;

public class PluginUtil {
    /** The default storage location of plugins */
    private static final String defaultPluginDir = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "plugins" + File.separator;

    private static Vector<String> additionalPluginDirList = new Vector<String>();
    
    private static URLClassLoader pluginDirClassLoader = null;
    
    public static final String getDefaultPluginDirectory() {
        return defaultPluginDir;
    }
    
    public static final Vector<String> getAdditionalPluginDirectories() {
        return additionalPluginDirList;
    }
    
    public static final void addPluginDirectory(final String dir) {
        additionalPluginDirList.add(dir);
    }
    
    public static final void resetAdditionalPluginDirectories() {
        additionalPluginDirList.removeAllElements();
    }
    
    /**
     * Returns whether the given class is a MIPAV/ImageJ plugin.
     * @param c A class.
     * @return True, if the given class is a MIPAV/ImageJ plugin.
     */
    public static final boolean isPluginClass(Class<?> c) {
        return c != null && (isMipavPluginClass(c) || isImageJPluginClass(c));
    }
    
    /**
     * Returns whether the given class is a MIPAV plugin.
     * @param c A class.
     * @return True, if the given class is a MIPAV plugin.
     */
    public static final boolean isMipavPluginClass(Class<?> c) {
        try {
            for (Class<?> inter : c.getInterfaces()) {
                if (inter.equals(PlugInAlgorithm.class) || inter.equals(PlugInGeneric.class) || inter.equals(PlugInFile.class)
                        || inter.equals(PlugIn.class) || inter.equals(PlugInView.class)) {
                    return true;
                }
            }
        } catch (Exception e) {
            System.err.println(c.getName());
        }
        
        return false;
    }
    
    /**
     * Returns whether the given class is an ImageJ plugin.
     * @param c A class.
     * @return True, if the given class is an ImageJ plugin.
     */
    public static final boolean isImageJPluginClass(Class<?> c) {
        for (Class<?> inter : c.getInterfaces()) {
            if (inter.equals(ij.plugin.PlugIn.class) || inter.equals(ij.plugin.filter.PlugInFilter.class) || c.getSuperclass().equals(ij.plugin.frame.PlugInFrame.class)) {
                return true;
            }
        }
        
        return false;
    }
    
    private static boolean helpPluginSearch(File f, String className) {
        boolean found = false;
        File[] fList = f.listFiles();
        String fileName;
        for (int i = 0; i < fList.length; i++) {
            if (fList[i].isDirectory()) {
                found = helpPluginSearch(fList[i], className);
            } else if (fList[i].getName().contains(".class")) {
                fileName = fList[i].getName().substring(0, fList[i].getName().indexOf(".class"));
                found = fileName.equals(className);
            }

            if (found) {
                return found; // true
            }
        }
        return found; // false
    }
    
    public static ClassLoader getPluginClassLoader() {
        // TODO check that no new dirs have been added since the classloader was created
        if (pluginDirClassLoader == null) {
            ArrayList<URL> urlList = new ArrayList<URL>();
            
            try {
                urlList.add(new File(getDefaultPluginDirectory()).toURI().toURL());
            
                for (String dirStr : getAdditionalPluginDirectories()) {
                    urlList.add(new File(dirStr).toURI().toURL());
                }
            } catch (MalformedURLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            
            pluginDirClassLoader = new URLClassLoader(urlList.toArray(new URL[urlList.size()]), PluginUtil.class.getClassLoader());
        }
        
        return pluginDirClassLoader;
    }
    
    public static Class<?> loadPluginClass(final String plugInName) {
        return loadPluginClass(plugInName, null, plugInName);
    }
    
    public static Class<?> loadPluginClass(final String plugInName, final Object source, final String command) {
        try {
            ClassLoader cl = null;

            if (source != null && source instanceof JMenuItem) {
                if ( ((JMenuItem) source).getToolTipText() != null && ((JMenuItem) source).getToolTipText().contains(".jar")) {
                    // should really be full path
                    final URL[] url = new URL[] {new URL("jar:file:" + ((JMenuItem) source).getToolTipText() + "!/")};
                    cl = URLClassLoader.newInstance(url);
                }
            }

            if (cl == null) {
                cl = getPluginClassLoader();
            }
            
            return cl.loadClass(plugInName);
        } catch (final Exception e) {
            MipavUtil.displayError("Unable to load plugin: " + command);
            return null;
        }
    }
    
    public static Object loadPlugin(final String plugInName) {
        return loadPlugin(plugInName, null, plugInName);
    }
    
    public static Object loadPlugin(final String plugInName, final Object source, final String command) {
        Object thePlugIn = null;

        try {
            Class<?> pluginClass = loadPluginClass(plugInName, source, command);
            
            thePlugIn = pluginClass.newInstance();

            return thePlugIn;
        } catch (final Exception e) {
            MipavUtil.displayError("Unable to load plugin: " + command);
            return null;
        }
    }

    public static String getPluginInterfaces(final Class<?> plugin) {
        String interName = new String();
        final Class<?>[] interList = plugin.getInterfaces();
        for (final Class<?> element : interList) {
            if (element.getName().contains("PlugIn")) {
                interName = element.getName().substring(element.getName().indexOf("PlugIn"));
            }
        }

        if (interName.length() == 0 && plugin.getSuperclass() != null) {
            return getPluginInterfaces(plugin.getSuperclass());
        } else {
            return interName;
        }
    }

    /**
     * Determines whether the <code>className</code> is in the plugin folder.
     * 
     * @param className
     * @return
     */
    public static boolean isInPluginFolder(String className) {
        boolean found = false;
        File plugin = new File(PluginUtil.getDefaultPluginDirectory());
        File[] fList = plugin.listFiles();
        String fileName;
        for (int i = 0; i < fList.length; i++) {
            if (fList[i].isDirectory()) {
                found = helpPluginSearch(fList[i], className);
            } else if (fList[i].getName().contains(".class")) {
                fileName = fList[i].getName().substring(0, fList[i].getName().indexOf(".class"));
                found = fileName.equals(className);
            }

            if (found) {
                return found; // true
            }
        }
        return found; // false
    }

    /**
     * Determines whether <code>c</code> is in the current plugin folder.
     * 
     * @param c
     * @return
     */
    public static boolean isInPluginFolder(Class<?> c) {
        boolean found = isInPluginFolder(c.getSimpleName());

        if (found) {
            return found; // true
        }

        File plugin = new File(PluginUtil.getDefaultPluginDirectory());
        URL fileLoc = null;
        try {
            fileLoc = c.getProtectionDomain().getCodeSource().getLocation();
        } catch (NullPointerException e) {
            return false;
        }

        try {
            if (fileLoc.toString().contains(plugin.toURI().toURL().toString())) {
                return true;
            } else {
                return false;
            }
        } catch (MalformedURLException e) {
            // pluginDir needs to specify a valid location
            e.printStackTrace();
            return false;
        }
    }
}
