package gov.nih.mipav.model.algorithms.itk.autoItk;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Component;
import javax.swing.*;

import java.util.jar.*;
import java.util.*;
import java.lang.reflect.*;
import java.io.*;
import java.util.Enumeration;
import InsightToolkit.*;

class FilterRecord implements Serializable
{
    public enum FilterState {
        NEW ("New"), 
        REMOVED ("Removed"), 
        NORMAL ("");

        private final String m_Name;

        FilterState(String name) {
            this.m_Name = name;
        }

        String getName() { return m_Name; }

    }

    static final long serialVersionUID = 4242L;
    public String m_Name;
    public boolean m_Active;
    public transient FilterState m_State;

    public FilterRecord() { m_Name = ""; m_Active = false; m_State = FilterState.NORMAL;}
   
    public FilterRecord(String name, boolean active) {
        m_Name = name;
        m_Active = active;
        m_State = FilterState.NORMAL;
    }
}


/**
 * @author helser
 * Controller that loads image-to-image filters from the Insight Toolkit,
 * and automatically makes them available in a menu. 
 */
public class AutoItkLoader implements ActionListener {
    private Component m_Frame;
    private JMenu m_Submenu;
    private List<FilterRecord> m_FilterList;
    private final String LIST_SERIALIZE_FILE = "itk_list.ser";
	
    /**
     * @param frame the parent frame containing the menu, parent for dialogs.
     * @param submenu Menu to populate with a list of ITK filters
     */
    public AutoItkLoader(Component frame, JMenu submenu)
    {
        m_Frame = frame;
        m_FilterList = combineFileJarList();
        
        m_Submenu = submenu;
        setMenu();
    }

    public void actionPerformed(ActionEvent action_evt) 
    {
        String name = action_evt.getActionCommand();
        if (name.equals("ConfigJar")) {
            if (m_FilterList != null) {
            	// make config dialog.
            	boolean do_set = ItkFilterSelectorDialog.showDialog(
                                        m_Frame,
                                        m_Frame,	
                                        "Choose filters to appear:",	
                                        "Complete ITK Filter List",
                                        m_FilterList);
                System.out.println("Do set " + do_set);
                if (do_set) {
                    setMenu();
                    writeListToFile();
                }
            }
        } else {
            String msg = new String();
            // all other actions are filters...
            Class<?> cls = null;
            try {
                cls = Class.forName("InsightToolkit.itk" + name + "FilterUC2UC2");
                //System.out.println("Found " + cls.getName());
                //count++;
                msg = "Found "+ cls.getName() + "\n";
                //msg += printSetMethods(cls);
            }
            catch (ClassNotFoundException cnfe) {
                //System.out.println("No luck, " + jef_name);
                msg = "Not found " + name;
                JOptionPane.showMessageDialog(m_Frame,
                                              msg, "Filter class result",
                                              JOptionPane.PLAIN_MESSAGE);
                return;
            }
            // Create a filter object.
            Object filter_pointer_obj = createFilterObj(cls);
            if (filter_pointer_obj == null) return;
            Object filter_obj = invokeMethod("GetPointer", filter_pointer_obj);

            boolean do_set = ItkFilterRunDialog.showDialog(
                                        m_Frame,
                                        m_Frame,	
                                        "Set parameters for filter:",	
                                        name + " Filter",
                                        filter_obj);
            System.out.println("Do set " + do_set);
            if (do_set) {
                // execute filter
            }
        }
    }

    /** 
     * Produce a list of the 'Set' methods of a filter class.
     * @param cls
     * @return list of Set method objects
     */
    public static List<Method> findSetMethods(Class<?> cls) {
        //String out = "";
        List<Method> out_list = new ArrayList<Method>();
        boolean found_one = false;
        for (; cls != null; cls = cls.getSuperclass()) {
            // ImageToImageFilter seems to be a general base class for filters.
            // However, there is a different one for each input/output type.
            if (cls.getSimpleName().contains("ImageToImage")) {
                break;
            }
            //out += "   " + cls.getSimpleName() + "\n";
            for (Method mthd : cls.getDeclaredMethods()) {
                if (mthd.getName().startsWith("Set")) {
                    found_one = true;
                    out_list.add(mthd);
                    /*
                    out += "      " + mthd.getName() + " ( ";
                    int params = 0;
                    for (Class<?> param_cls : mthd.getParameterTypes()) {
                        if (params != 0) out += ", ";
                        out += param_cls.getName();
                        params++;
                    }	
                    out += " )\n";
                    */
                }
            }
        }
        return (found_one ? out_list : null);
    }

    /**
     * Adds the current list of filters to our menu
     */
    void setMenu() 
    {
        if (m_Submenu == null) return;
        m_Submenu.removeAll();
        for (JComponent menuItem: getMenuItems()) {
            m_Submenu.add(menuItem);
        }
    }
    
    /**
     * Generate all menu items 
     * @return list of menu items
     */
    JComponent[] getMenuItems()
    {
        if (m_FilterList == null) {
            m_FilterList = combineFileJarList();
        }
        List<JComponent> item_list = new ArrayList<JComponent>();
        JMenuItem item = null;
        if (m_FilterList != null) {
            for(Iterator<FilterRecord> it = m_FilterList.iterator(); it.hasNext(); ) {
                FilterRecord filter_rec = it.next();
                if (filter_rec.m_Active && filter_rec.m_State != FilterRecord.FilterState.REMOVED) {
                    item = new JMenuItem(filter_rec.m_Name);
                    item.setActionCommand(filter_rec.m_Name);
                    item.addActionListener(this);
                    item_list.add(item);
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
        return item_list.toArray(new JComponent[] { });
    }
    
    /**
     * Utility method to change the m_State member of each item in a FilterRecord list.
     * @param fr_list
     * @param state new state value.
     */
    private void setListState(List<FilterRecord> fr_list, FilterRecord.FilterState state)
    {
        for(Iterator<FilterRecord> it = fr_list.iterator(); it.hasNext(); ) {
            it.next().m_State = state;
        }
    }

    /**
     * Utility method to find a FilterRecord with a matching name in supplied list.
     * @param fr_list list to search.
     * @param name to match.
     * @return matching FilterRecord.
     */
    private FilterRecord matchListName(List<FilterRecord> fr_list, String name)
    {
        for(Iterator<FilterRecord> it = fr_list.iterator(); it.hasNext(); ) {
            FilterRecord fr = it.next();
            if (fr.m_Name.equals(name)) return fr;
        }
        return null;
    }
	
    // combine information from the current jar, and the saved records with
    // filters turned on/off.
    private List<FilterRecord> combineFileJarList() 
    {
        List<FilterRecord> jar_list = listFromJar();
        if (jar_list == null) {
            // what's the useful action here?
            return listFromFile();
        }
        
        List<FilterRecord> file_list = listFromFile();
        if (file_list == null) {
            setListState(jar_list, FilterRecord.FilterState.NEW);
            return jar_list;
        }

        // If a record is in the jar and not the file, it's new, and active.
        // In both, it's normal, and active is determined by the file.
        // If a record is in the file and not the jar, it's removed, and inactive. 
        for(Iterator<FilterRecord> it = jar_list.iterator(); it.hasNext(); ) {
            FilterRecord fr_jar = it.next();
            FilterRecord fr_file = matchListName(file_list, fr_jar.m_Name);
            if (fr_file != null) {
                fr_jar.m_State = FilterRecord.FilterState.NORMAL;
                fr_jar.m_Active = fr_file.m_Active;
            } else {
                fr_jar.m_State = FilterRecord.FilterState.NEW;
                fr_jar.m_Active = true;
            }
        }                
        // traverse file list, looking for ones that have been removed from jar
        for(Iterator<FilterRecord> it = file_list.iterator(); it.hasNext(); ) {
            FilterRecord fr_file = it.next();
            FilterRecord fr_jar = matchListName(jar_list, fr_file.m_Name);
            if (fr_jar == null) {
                fr_file.m_State = FilterRecord.FilterState.REMOVED;
                fr_file.m_Active = false;
                jar_list.add(fr_file);
            }
        }                

        return jar_list;
    }

    /**
     * @return FilterRecords stored in our file, with active state.
     */
    private List<FilterRecord> listFromJar()
    {
        JarFile jf = null;
        Manifest jf_manifest =null;
        try {
            String class_path_key = "java.class.path";
            String class_path = System.getProperty(class_path_key);
            //System.out.println(class_path);
            
            String jar_filename = null;
            for (String fn : class_path.split(";") ) {
                if (fn.endsWith("InsightToolkit.jar")) {
                    jar_filename = fn;
                    System.out.println("Found itk jar: " + jar_filename);
                    break;
                }
            }
            if (jar_filename != null) {
                jf = new JarFile(jar_filename);

                jf_manifest = jf.getManifest();
            }
       
        } 
        catch (FileNotFoundException fnfe) {
            System.out.println("listFromJar can't find jar on class path");
        }
        catch (IOException io_exception) {
            System.out.println("listFromJar can't read jar: " + io_exception.toString());
        }

        if (jf_manifest == null || jf == null ) return null;

        JarEntry je = null;
        String last_filter_str = "";
        int count = 0;
        List<FilterRecord> filter_list = new ArrayList<FilterRecord>();
        for (Enumeration<JarEntry> e = jf.entries(); e.hasMoreElements(); ) {
            je = e.nextElement();
            File jef = new File(je.getName());
            String jef_name = jef.getName();
            int index = jef_name.lastIndexOf(".class");
            if (index > 0) {
                jef_name = jef_name.substring(0, index);
            } else {
                // not interested in names that don't end in .class
                continue;
            }
            index = jef_name.lastIndexOf("_Pointer");
            if (index > 0) continue;

            // only list classes that start with "itk"
            index = jef_name.indexOf("itk");
            if (index != 0) continue;
            // only concentrate on Filters for now.
            String filter_str = "Filter";
            index = jef_name.indexOf(filter_str);
            if (index <= 0) continue;
            String short_jef_name = jef_name.substring(0, index);
            if (short_jef_name.equals(last_filter_str)) continue;
            last_filter_str = short_jef_name;

                   
            Class<?> cls = null;
            try {
                cls = Class.forName("InsightToolkit." + jef_name + "UC2UC2");
                //System.out.println("Found " + cls.getName());
                count++;
            }
            catch (ClassNotFoundException cnfe) {
                //System.out.println("No luck, " + jef_name);
                continue;
            }
            
            // remove "itk" from the front.
            filter_list.add(new FilterRecord(short_jef_name.substring(3), true));

            // XXX Testing
            if (filter_list.size() == 3) {
                filter_list.add(new FilterRecord("AddNewTestImage", true));
            }
            // try setting up an complete filter call, for this filter:
            //if (jef_name.indexOf("MedianImageFilter") < 0) continue;
       
            //printAncestors(cls);
            //printSetMethods(cls);
            //invokeClass(cls, argv[0], argv[1]);
            // one filter.
            //break;

        }
    
        System.out.println("Unique filters: " + count);
        return filter_list;
    }

    private List<FilterRecord> listFromFile()
    {
        List<?> in_list = null;
        ObjectInputStream in = null;
        try {
            in = new ObjectInputStream(new
                                       BufferedInputStream(new FileInputStream(LIST_SERIALIZE_FILE)));
            in_list = (ArrayList<?>)in.readObject();
        }
        catch (FileNotFoundException fnfe) {
            // nothing, ok for file not to exist yet.
        }
        catch (IOException io_exception) {
            System.out.println("listFromFile " + io_exception.toString());

        }
        catch (ClassNotFoundException cnfe) {
            System.out.println("listFromFile " + cnfe.toString());
        } finally {
            try {
                if (in != null) in.close();
            } catch (IOException io_exception) { }
        }

        if (in_list == null) return null;

        // making another list of the correct type avoids compiler warnings.
        ArrayList<FilterRecord> ret_list = new ArrayList<FilterRecord>();

        // Filter state is transient, so comes in un-initialized i.e. null !! 
        // Assume that all filters are normal.
        for(Iterator<?> it = in_list.iterator(); it.hasNext(); ) {
            FilterRecord filter_name = (FilterRecord) it.next();
            filter_name.m_State = FilterRecord.FilterState.NORMAL;
            ret_list.add(filter_name);

            // XXX Testing
            if (ret_list.size() == 3) {
                ret_list.add(new FilterRecord("RemoveTestImage", true));
            }
        }
        return ret_list;
    }

    // Remove all REMOVED filter records, then write the list to a file.
    private boolean writeListToFile()
    {
        for(Iterator<FilterRecord> it = m_FilterList.iterator(); it.hasNext(); ) {
            if (it.next().m_State == FilterRecord.FilterState.REMOVED) {
                it.remove();
            }
        }

        ObjectOutputStream out = null;
        try {
            out = new ObjectOutputStream(new
                                         BufferedOutputStream(new FileOutputStream(LIST_SERIALIZE_FILE)));

            out.writeObject(m_FilterList);
        }
        catch (IOException io_exception) {
            System.out.println("writeListToFile " + io_exception.toString());
        } finally {
            try {
                if (out != null) out.close();
            } catch (IOException io_exception) { }
        }
        return true;
    }

    public static Object createFilterObj(Class<?> cls)
    {
        // Each filter class in ITK has a corresponding Pointer type,
        // so filter T is created:
        // T_Pointer = T.T_New();
        // T is passed in as the template arg to Class, so we need to 
        // construct T_Pointer, and the New method.
      
        String pointer_class_name = cls.getName() + "_Pointer";
        Class<?> t_ptr_cls = null;
        try {
            t_ptr_cls = Class.forName(pointer_class_name);
            System.out.println("Found " + t_ptr_cls.getName());
        }
        catch (ClassNotFoundException cnfe) {
            System.out.println("No luck, " + pointer_class_name);
            return null;
        }


        String static_constructor_name = cls.getSimpleName() + "_New";
        // NOTE: this returns a filter_Pointer, not a filter. 
        // call GetPointer method after saving to get the filter itself.
        return invokeMethod(static_constructor_name, (Object)null, cls);
    }

    // no class, no arg method
    public static Object invokeMethod(String method_name, Object obj) 
    {
        return invokeMethod(method_name, obj, null, null);
    }

    // no arg method
    public static Object invokeMethod(String method_name, Object obj, 
                                      Class<?> cls) 
    {
        return invokeMethod(method_name, obj, cls, null);
    }

    public static Object invokeMethod(String method_name, Object obj, 
                                      Class<?> cls, Object obj_arg)
    {
        assert(obj != null || cls != null);

        if (cls == null) {
            cls = obj.getClass();
        }
        Method m = null;
        Class<?> class_args [] = null;
        if (obj_arg != null) {
            try {
                // See if obj_arg passed in has a TYPE field, which means we should
                // retrieve the primitive Class object for it from that Field.
                Field f = obj_arg.getClass().getField("TYPE");
                class_args = new Class<?> [] { (Class<?>)f.get(obj_arg) } ;
            } catch (NoSuchFieldException nsfe) {
            } catch (IllegalAccessException iae) {
            }
        }

        try {
            // get normal class for arg, if not filled in above.
            if (obj_arg != null && class_args == null) {
                class_args = new Class<?> [] { obj_arg.getClass() };
             }
            m = cls.getMethod(method_name, class_args);
        }
        catch (NoSuchMethodException nsme) {
            System.out.println("No luck, " + nsme.toString());
            return null;
        }
        //m.setAccessible(true);
        //int mods = m.getModifiers();
        //       if (m.getReturnType() != void.class || !Modifier.isStatic(mods) ||
        //           !Modifier.isPublic(mods)) {
        //          throw new NoSuchMethodException("main");
        //       }
        Object ret_obj = null;
        try {
            // if obj_args is null, no args.
            Object[] obj_args = (obj_arg == null ? null : 
                                 new Object[] { obj_arg });
            // if obj is null, static method.
            ret_obj = m.invoke(obj, obj_args);
        } catch (IllegalAccessException iae) {
            // If this happens, call m.setAccessible(true);, then it won't
            System.out.println("Method invoke access problem, " + method_name);
            return null;
        }
        catch (InvocationTargetException ite) {
            System.out.println("Method invoke " + method_name + ", " + ite);
            return null;
        }
        System.out.println("Method " + method_name + " return " + ret_obj);
        return ret_obj;
    }

}
