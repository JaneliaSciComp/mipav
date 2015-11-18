package gov.nih.mipav.model.algorithms.itk;


import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogItkFilter;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;


/**
 * Controller that loads image-to-image filters from the Insight Toolkit, and automatically makes them available in a
 * menu.
 * 
 * @author Geometric Tools
 */
public class AutoItkLoader implements ActionListener {

    /**
     * Frame to parent dialogs, and to retrieve ModelImage data from.
     */
    private final ViewJFrameImage m_Frame;

    /**
     * Menu we control, filled with filters and the config dialog.
     */
    private final JMenu m_Submenu;

    /**
     * List of filters, with active state and status.
     */
    private List<FilterRecordItk> m_FilterList;

    /**
     * Full path of InsightToolkit.jar, if we locate it.
     */
    private String m_ItkJarFilename = new String("");

    /**
     * Name of the file we serialize the m_FilterList to.
     */
    private final String LIST_SERIALIZE_FILE = "itk_list.ser";

    // can't change the itk java output behavior yet:
    // private static ItkConsoleOutput ms_OutputWindow = null;

    /**
     * for each integer returned by getType(), what sequence matches itk i/o types. X says no match (complex numbers.)
     */
    private static final String[] MODELIMAGE_TYPE_TABLE = {"B", "SC", "UC", "SS", "US", "SI", "SL", "F", "D", "UC", "US", "F", "X", "X", "UI"};

    public static String getItkModelImageString(final int model_image_type) {
        if (model_image_type < 0 || model_image_type >= AutoItkLoader.MODELIMAGE_TYPE_TABLE.length) {
            return "";
        }
        return AutoItkLoader.MODELIMAGE_TYPE_TABLE[model_image_type];
    }

    /**
     * @param frame the parent frame containing the menu, parent for dialogs.
     * @param submenu Menu to populate with a list of ITK filters
     */
    public AutoItkLoader(final ViewJFrameImage frame, final JMenu submenu) {
        m_Frame = frame;
        m_FilterList = combineFileJarList();

        m_Submenu = submenu;
        setMenu();
        // initialize output from ITK library to go to console.
        // Doesn't work, posted to itk-users, 2/25/08
        // if (ms_OutputWindow == null) {
        // ms_OutputWindow = new ItkConsoleOutput();
        // itkOutputWindow.SetInstance(ms_OutputWindow);
        // }
    }

    // invoke a config dialog or filter dialog, via menu item. Eclipse wants @Overide on this....
    @Override
    public void actionPerformed(final ActionEvent action_evt) {
        final String name = action_evt.getActionCommand();
        if (name.equals("ConfigJar")) {
            if (m_FilterList != null) {
                // make config dialog.
                final boolean do_set = ItkFilterSelectorDialog.showDialog(m_Frame, m_Frame, "Choose filters to appear:", "Complete ITK Filter List",
                        m_FilterList);
                // System.out.println("Do set " + do_set);
                if (do_set) {
                    setMenu();
                    writeListToFile();
                }
            }
        } else {
            // all other actions are filters...
            final FilterRecordItk fr = matchListName(m_FilterList, name);
            final ModelImage model_image = m_Frame.getActiveImage();

            // create JDialogItkFilter, and forget, dialog is responsible
            // for getting and displaying result, if any.
            if (fr != null && model_image != null) {
                new JDialogItkFilter(m_Frame, model_image, fr);
            }

        }
    }

    /**
     * Produce a list of the 'Set' methods of a filter class, if this is a sub-class of an ImageToImage filter.
     * 
     * @param cls
     * @return list of Set method objects, or null if not useful.
     */
    public static List<Method> findSetMethods(Class<?> cls) {
        // if this IS the ImageToImage class, we don't want to use it.
        if (cls.getSimpleName().contains("ImageToImage")) {
            return null;
        }
        // String out = "";
        final List<Method> out_list = new ArrayList<Method>();
        // boolean found_one = false;
        boolean found_i2i_base = false;
        for (; cls != null; cls = cls.getSuperclass()) {
            // ImageToImageFilter seems to be a general base class for filters.
            // However, there is a different one for each input/output type.
            if (cls.getSimpleName().contains("ImageToImage")) {
                found_i2i_base = true;
                break;
            }
            // out += "   " + cls.getSimpleName() + "\n";
            for (final Method mthd : cls.getDeclaredMethods()) {
                if (mthd.getName().startsWith("Set")) {
                    // found_one = true;
                    out_list.add(mthd);
                }
            }
        }
        return (found_i2i_base ? out_list : null);
    }

    /**
     * Determine whether this cls is probably a valid ITK image to image filter which might be useful.
     * 
     * @param cls the filter type
     * @return true if it's probably useful.
     */
    private static boolean validFilterClass(Class<?> cls) {
        if (cls.getSimpleName().contains("ImageToImage")) {
            return false;
        }
        // Check if this is a base class for other filters -
        // if so, it will have no factory method - no static "_New" method.
        try {
            cls.getMethod(cls.getSimpleName() + "_New", (Class<?>[]) null);
        } catch (final NoSuchMethodException nsme) {
            return false;
        }
        // Check that it's derived from an ImageToImage filter of some type.
        boolean found_i2i_base = false;
        for (; cls != null; cls = cls.getSuperclass()) {
            if (cls.getSimpleName().contains("ImageToImage")) {
                found_i2i_base = true;
                break;
            }
        }
        if ( !found_i2i_base) {
            return false;
        }
        return true;
    }

    /**
     * Sets the current list of filters in our menu
     */
    void setMenu() {
        if (m_Submenu == null) {
            return;
        }
        m_Submenu.removeAll();
        for (final JComponent menuItem : getMenuItems()) {
            m_Submenu.add(menuItem);
        }
    }

    /**
     * Generate all menu items for filters and filter config dialog.
     * 
     * @return list of menu items, at least contains the config command.
     */
    JComponent[] getMenuItems() {
        if (m_FilterList == null) {
            m_FilterList = combineFileJarList();
        }
        final List<JComponent> item_list = new ArrayList<JComponent>();
        JMenuItem item = null;
        JMenu more_menu = null;
        int added_count = 0;
        if (m_FilterList != null) {
            for (final FilterRecordItk filter_rec : m_FilterList) {
                if (filter_rec.m_Active && filter_rec.m_State != FilterRecordItk.FilterState.REMOVED) {
                    item = new JMenuItem(filter_rec.m_Name);
                    item.setActionCommand(filter_rec.m_Name);
                    item.addActionListener(this);
                    if (added_count < 30) {
                        item_list.add(item);
                    } else {
                        if (more_menu == null) {
                            more_menu = new JMenu("More");
                            item_list.add(more_menu);
                        }
                        more_menu.add(item);
                    }
                    added_count++;
                }
            }
        }
        if (item_list.isEmpty()) {
            item_list.add(new JMenuItem("No active filters, please Update"));
        }
        item_list.add(new JSeparator());
        // add command to open the FilterSelection dialog.
        item = new JMenuItem("Update ITK filter list...");
        item.setActionCommand("ConfigJar");
        item.addActionListener(this);
        item_list.add(item);
        return item_list.toArray(new JComponent[] {});
    }

    /**
     * Utility method to change the m_State member of each item in a FilterRecordItk list.
     * 
     * @param fr_list
     * @param state new state value.
     */
    private void setListState(final List<FilterRecordItk> fr_list, final FilterRecordItk.FilterState state) {
        for (final FilterRecordItk filterRecordItk : fr_list) {
            filterRecordItk.m_State = state;
        }
    }

    /**
     * Utility method to find a FilterRecordItk with a matching name in supplied list.
     * 
     * @param fr_list list to search.
     * @param name to match.
     * @return matching FilterRecordItk.
     */
    private FilterRecordItk matchListName(final List<FilterRecordItk> fr_list, final String name) {
        for (final FilterRecordItk fr : fr_list) {
            if (fr.m_Name.equals(name)) {
                return fr;
            }
        }
        return null;
    }

    /**
     * combine information from the current jar, and the saved records with filters turned on/off, into a single list
     * reflecting active filters and NEW, NORMAL, REMOVED state.
     * 
     * @return list, or null if jar _and_ serialization file can't be read.
     */
    private List<FilterRecordItk> combineFileJarList() {
        final List<FilterRecordItk> jar_list = listFromJar();
        if (jar_list == null) {
            // what's the useful action here?
            return listFromFile();
        }

        final List<FilterRecordItk> file_list = listFromFile();
        if (file_list == null) {
            setListState(jar_list, FilterRecordItk.FilterState.NEW);
            return jar_list;
        }

        // If a record is in the jar and not the file, it's new, and active.
        // In both, it's normal, and active is determined by the file.
        // If a record is in the file and not the jar, it's removed, and inactive.
        for (final FilterRecordItk fr_jar : jar_list) {
            final FilterRecordItk fr_file = matchListName(file_list, fr_jar.m_Name);
            if (fr_file != null) {
                fr_jar.m_State = FilterRecordItk.FilterState.NORMAL;
                fr_jar.m_Active = fr_file.m_Active;
            } else {
                fr_jar.m_State = FilterRecordItk.FilterState.NEW;
                fr_jar.m_Active = true;
            }
        }
        // traverse file list, looking for ones that have been removed from jar
        for (final FilterRecordItk fr_file : file_list) {
            final FilterRecordItk fr_jar = matchListName(jar_list, fr_file.m_Name);
            if (fr_jar == null) {
                fr_file.m_State = FilterRecordItk.FilterState.REMOVED;
                fr_file.m_Active = false;
                jar_list.add(fr_file);
            }
        }

        return jar_list;
    }

    /**
     * Get the InsightToolkit.jar from the classpath, and search it for Filter classes.
     * 
     * @return FilterRecordItks stored in the jar, all active.
     */
    private List<FilterRecordItk> listFromJar() {
        JarFile jf = null;
        Manifest jf_manifest = null;
        String jar_filename = "";
        try {
            final String class_path_key = "java.class.path";
            final String class_path = System.getProperty(class_path_key);
            // System.out.println(class_path);

            for (final String fn : class_path.split(File.pathSeparator)) {
                if (fn.endsWith("InsightToolkit.jar")) {
                    jar_filename = fn;
                    Preferences.debug("\nFound itk jar: " + jar_filename + "\n", Preferences.DEBUG_FILEIO);
                    break;
                }
            }
            if (jar_filename != null) {
                jf = new JarFile(jar_filename);

                jf_manifest = jf.getManifest();
            }

        } catch (final FileNotFoundException fnfe) {
            Preferences.debug("AutoITK: listFromJar can't find jar on class path\n", Preferences.DEBUG_FILEIO);
        } catch (final IOException io_exception) {
            Preferences.debug("AutoITK: listFromJar can't read jar: " + io_exception.toString() + "\n", Preferences.DEBUG_FILEIO);
        }

        if (jf_manifest == null || jf == null) {
            return null;
        }

        m_ItkJarFilename = jar_filename;

        JarEntry je = null;
        // String last_filter_str = "";
        FilterRecordItk last_filter = null;
        int count = 0;
        final List<FilterRecordItk> filter_list = new ArrayList<FilterRecordItk>();
        for (final Enumeration<JarEntry> e = jf.entries(); e.hasMoreElements();) {
            je = e.nextElement();
            final File jef = new File(je.getName());
            String jef_name = jef.getName();
            int index = jef_name.lastIndexOf(".class");
            if (index > 0) {
                jef_name = jef_name.substring(0, index);
            } else {
                // not interested in names that don't end in .class
                continue;
            }
            // ignore _Pointer classes, since they match another class.
            if (jef_name.lastIndexOf("_Pointer") > 0) {
                continue;
            }

            // only list classes that start with "itk"
            index = jef_name.indexOf("itk");
            if (index != 0) {
                continue;
            }
            // remove "itk" from the front.
            jef_name = jef_name.substring(3);

            // only concentrate on Filters.
            final String filter_str = "Filter";
            final int filter_str_len = filter_str.length();
            index = jef_name.indexOf(filter_str);
            if (index <= 0) {
                continue;
            }

            // contains everything before "Filter"
            final String short_jef_name = jef_name.substring(0, index);

            // See if we can find a data type string.
            // If there's nothing after "Filter", go on.
            if (index + filter_str_len >= jef_name.length()) {
                continue;
            }
            final String type_str = jef_name.substring(index + filter_str_len);
            // ignore JNI classes, those are just underlying static impl classes.
            if (type_str.indexOf("JNI") >= 0) {
                continue;
            }
            // ignore types with _, probably D2D2_Superclass, or similar.
            if (type_str.indexOf("_") >= 0) {
                continue;
            }
            // ignore types that start with base or Base, superclass.
            if (type_str.startsWith("base") || type_str.startsWith("Base")) {
                continue;
            }

            // don't add a filter record until we get a type.

            Class<?> cls = null;
            try {
                cls = Class.forName("InsightToolkit.itk" + short_jef_name + "Filter" + type_str);
                // System.out.println("Found " + cls.getName());
            } catch (final ClassNotFoundException cnfe) {
                Preferences.debug("AutoITK: Jar No luck, " + jef_name + "\n", Preferences.DEBUG_FILEIO);
                continue;
            }

            if (last_filter == null || !short_jef_name.equals(last_filter.m_Name)) {
                // test to see if this new class is a useful ImageToImage filter,
                if ( !AutoItkLoader.validFilterClass(cls)) {
                    continue;
                }
                last_filter = new FilterRecordItk(short_jef_name, true);
                filter_list.add(last_filter);
                count++;
                // System.out.println(short_jef_name);
            }
            // add type string.
            last_filter.m_IOType.add(type_str);

            // Some debugging info to print, for a specific filter:
            // if (jef_name.indexOf("MedianImageFilter") < 0) continue;

            // printAncestors(cls);
            // printSetMethods(cls);
            // stop after one filter.
            // break;

        }

        // System.out.println("Unique filters: " + count);
        return filter_list;
    }

    /**
     * Generate a list of FilterRecordItk from our default serialized file.
     * 
     * @return list, or null if file doesn't yet exist or is not readable or compatible.
     */
    private List<FilterRecordItk> listFromFile() {
        List<?> in_list = null;
        ObjectInputStream in = null;
        final String list_ser_file = m_ItkJarFilename == null ? LIST_SERIALIZE_FILE : new File(new File(m_ItkJarFilename).getParent(), LIST_SERIALIZE_FILE)
                .getPath();
        try {
            in = new ObjectInputStream(new BufferedInputStream(new FileInputStream(list_ser_file)));
            in_list = (ArrayList<?>) in.readObject();
        } catch (final FileNotFoundException fnfe) {
            // nothing, ok for file not to exist yet.
        } catch (final IOException io_exception) {
            Preferences.debug("AutoITK: listFromFile " + io_exception.toString() + "\n", Preferences.DEBUG_FILEIO);

        } catch (final ClassNotFoundException cnfe) {
            System.out.println("AutoITK: listFromFile " + cnfe.toString() + "\n");
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (final IOException io_exception) {}
        }

        if (in_list == null) {
            return null;
        }

        // making another list of the correct type avoids compiler warnings.
        final ArrayList<FilterRecordItk> ret_list = new ArrayList<FilterRecordItk>();

        // Filter state is transient, so comes in un-initialized i.e. null !!
        // Assume that all filters are normal.
        for (final Object name : in_list) {
            final FilterRecordItk filter_name = (FilterRecordItk) name;
            filter_name.m_State = FilterRecordItk.FilterState.NORMAL;
            ret_list.add(filter_name);

        }
        return ret_list;
    }

    /**
     * Remove all REMOVED filter records, then write the list to a file.
     * 
     * @return true on success
     */
    private boolean writeListToFile() {
        for (final Iterator<FilterRecordItk> it = m_FilterList.iterator(); it.hasNext();) {
            if (it.next().m_State == FilterRecordItk.FilterState.REMOVED) {
                it.remove();
            }
        }

        boolean ret = true;
        ObjectOutputStream out = null;
        final String list_ser_file = m_ItkJarFilename == null ? LIST_SERIALIZE_FILE : new File(new File(m_ItkJarFilename).getParent(), LIST_SERIALIZE_FILE)
                .getPath();
        try {
            out = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(list_ser_file)));

            out.writeObject(m_FilterList);
        } catch (final IOException io_exception) {
            Preferences.debug("AutoITK: writeListToFile " + io_exception.toString() + "\n", Preferences.DEBUG_FILEIO);
            ret = false;
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (final IOException io_exception) {}
        }
        return ret;
    }

    /**
     * Given a class representing an Itk filter object, create an instance. Uses itk smart pointers.
     * 
     * @param cls the filter's type.
     * @return instance of the filter class.
     */
    public static Object createFilterObj(final Class<?> cls) {
        // Each filter class in ITK has a corresponding Pointer type,
        // obstained from a static factory method. So filter T is created:
        // T_Pointer = T.T_New();
        // The type T is passed in as the arg cls, so we need to
        // construct T_Pointer, and the New method.

        final String pointer_class_name = cls.getName() + "_Pointer";
        // Class<?> t_ptr_cls = null;
        try {
            // t_ptr_cls = Class.forName(pointer_class_name);
            Class.forName(pointer_class_name);
            // System.out.println("Found " + t_ptr_cls.getName());
        } catch (final ClassNotFoundException cnfe) {
            // System.out.println("No luck, " + pointer_class_name);
            return null;
        }

        final String static_constructor_name = cls.getSimpleName() + "_New";
        // NOTE: this returns a filter_Pointer, not a filter.
        // call GetPointer method after saving to get the filter itself.
        return AutoItkLoader.invokeMethod(static_constructor_name, (Object) null, cls);
    }

    /**
     * Factory for itk image container, that keeps a smart pointer around so the image data doesn't get garbage
     * collected.
     * 
     * @param model_image_type ModelImage data type.
     * @return null if type is not available
     */
    public static PItkImage2 createItkImage2(final int model_image_type) {
        final PItkImage2 ret = new PItkImage2(model_image_type);
        if (ret.img() != null) {
            return ret;
        }
        return null;
    }

    /**
     * @see createItkImage2
     * @param model_image_type
     * @return
     */
    public static PItkImage3 createItkImage3(final int model_image_type) {
        final PItkImage3 ret = new PItkImage3(model_image_type);
        if (ret.img() != null) {
            return ret;
        }
        return null;
    }

    /**
     * Invoke a method on an object, without throwing exceptions. no arg method
     * 
     * @param method_name Name of the method
     * @param obj invoke this object's method
     * @return result of the method, might be null (including for void return).
     */
    public static Object invokeMethod(final String method_name, final Object obj) {
        return AutoItkLoader.invokeMethod(method_name, obj, null, (Object[]) null);
    }

    /**
     * Invoke a method by name without throwing exceptions.
     * 
     * @param method_name
     * @param obj If non-null, invoke this object's method. If null, static class method.
     * @param cls If obj is non-null, ignored. Otherwise the class for static method.
     * @param obj_arg Variable number of args, passed as objects.
     * @return result of the method, might be null (including for void return).
     */
    public static Object invokeMethod(final String method_name, final Object obj, Class<?> cls, final Object... obj_arg) {
        assert (obj != null || cls != null);

        if (cls == null) {
            cls = obj.getClass();
        }
        Method m = null;

        // Set up list of arguments. In ITK, always 0 or 1 arg.
        Class<?> class_args[] = null;
        if (obj_arg != null && obj_arg.length > 0) {
            class_args = new Class<?>[obj_arg.length];
            for (int i = 0; i < obj_arg.length; i++) {
                class_args[i] = null;
                try {
                    // See if obj_arg passed in has a TYPE field, which means we should
                    // retrieve the primitive Class object for it from that Field.
                    final Field f = obj_arg[i].getClass().getField("TYPE");
                    class_args[i] = (Class<?>) f.get(obj_arg);
                } catch (final NoSuchFieldException nsfe) {} catch (final IllegalAccessException iae) {}

                if (class_args[i] == null) {
                    // get normal class for arg, if not filled in above.
                    class_args[i] = obj_arg[i].getClass();
                }
            }
        }
        try {
            m = cls.getMethod(method_name, class_args);
        } catch (final NoSuchMethodException nsme) {
            // Expect some GetElement calls to fail, because of int/long as arg.
            // Expect some GetX calls for finding default values to fail.
            if ( !method_name.startsWith("Get")) {
                Preferences.debug("AutoITK: No luck, " + nsme.toString() + "\n", Preferences.DEBUG_FILEIO);
            }
            return null;
        }
        return AutoItkLoader.invokeMethod(obj, m, obj_arg);
    }

    /**
     * Invoke an existing method on an object instance. Avoid throwing exceptions.
     * 
     * @param obj May be null for static method.
     * @param m method to invoke.
     * @param obj_arg variable list of args.
     * @return method result.
     */
    public static Object invokeMethod(final Object obj, final Method m, final Object... obj_arg) {
        Object ret_obj = null;
        try {
            // if obj_arg is null, no args. Otherwise, array of args.
            // if obj is null, static method.
            ret_obj = m.invoke(obj, obj_arg);
        } catch (final IllegalAccessException iae) {
            // If this happens, call m.setAccessible(true);, then it won't
            Preferences.debug("AutoITK: Method invoke access problem, " + m.getName() + "\n", Preferences.DEBUG_FILEIO);
            return null;
        } catch (final InvocationTargetException ite) {
            Preferences.debug("AutoITK: Method invoke " + m.getName() + ", threw an exception." + "\n", Preferences.DEBUG_FILEIO);
            return null;
        } catch (final IllegalArgumentException iae) {
            Preferences.debug("AutoITK: Method invoke " + m.getName() + ", " + iae + "\n", Preferences.DEBUG_FILEIO);
        }
        // System.out.println("Method " + m.getName() + " return " + ret_obj);
        return ret_obj;
    }

}
