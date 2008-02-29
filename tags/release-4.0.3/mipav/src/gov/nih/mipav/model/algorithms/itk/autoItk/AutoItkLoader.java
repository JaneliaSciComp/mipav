package gov.nih.mipav.model.algorithms.itk.autoItk;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Component;
import java.awt.Dimension;
import javax.swing.*;

import java.util.jar.*;
import java.util.*;
import java.lang.reflect.*;
import java.io.*;
import java.util.Enumeration;
import InsightToolkit.*;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

/** Struct that holds public data about an itk filter.
 * @author Geometric Tools
 */
class FilterRecord implements Serializable
{
    /**
     * Normal filters appear in the jar and the saved record,
     * New filters are only in the jar,
     * Removed filters are obsolete: only in the saved record.
     */
    public enum FilterState {
        NEW ("New"), 
        REMOVED ("Removed"), 
        NORMAL ("");

        private final String m_Name;

        FilterState(String name) {
            this.m_Name = name;
        }

        /** Used to provide the user some info about the filter.
         * @return a description for New or Removed filters.
         */
        String getName() { return m_Name; }

    }

    /**
     * Avoid compiler warning. Change if class fields change.
     */
    static final long serialVersionUID = 4243L;
    /**
     * Name directly from the Itk class, InsightToolkit.itkBinaryDilateImageFilterF2F2 becomes BinaryDilateImage
     */
    public String m_Name;
    /**
     * Is this filter shown in the menu. Default yes, user can turn them off.
     */
    public boolean m_Active;
    /**
     * List of i/o types for the filter. F2F2 is a typical one. 
     */
    public ArrayList<String> m_IOType;
    /**
     * New/removed state. Not serialized. 
     */
    public transient FilterState m_State;

    /**
     * Default constructor, unused.
     */
    public FilterRecord() { 
        m_Name = ""; m_Active = false; 
        m_State = FilterState.NORMAL;
        m_IOType = new ArrayList<String>();
    }
   
    /** Create a record, NORMAL filter state.
     * @param name Filter name
     * @param active Show in menu
     */
    public FilterRecord(String name, boolean active) {
        m_Name = name;
        m_Active = active;
        m_State = FilterState.NORMAL;
        m_IOType = new ArrayList<String>();
    }
}


/**
 * Controller that loads image-to-image filters from the Insight Toolkit,
 * and automatically makes them available in a menu. 
 * @author Geometric Tools
 */
public class AutoItkLoader implements ActionListener {
    /**
     * Frame to parent dialogs, and to retrieve ModelImage data from.
     */
    private ViewJFrameImage m_Frame;
    /**
     * Menu we control, filled with filters and the config dialog.
     */
    private JMenu m_Submenu;
    /**
     * List of filters, with active state and status.
     */
    private List<FilterRecord> m_FilterList;
    /**
     * Name of the file we serialize the m_FilterList to.
     */
    private final String LIST_SERIALIZE_FILE = "itk_list.ser";
    // can't change the itk java output behavior yet:
    //private static ItkConsoleOutput ms_OutputWindow = null;

    /** for each integer returned by getType(), what sequence matches itk i/o types. X says no match (complex) */
    private static final String[] MODELIMAGE_TYPE_TABLE = { "B", "SC", "UC", "SS", "US", "SI", "SL", "F", "D", 
                                                            "UC", "US", "F", "X", "X" , "UI" };


    /**
     * @param frame the parent frame containing the menu, parent for dialogs.
     * @param submenu Menu to populate with a list of ITK filters
     */
    public AutoItkLoader(ViewJFrameImage frame, JMenu submenu)
    {
        m_Frame = frame;
        m_FilterList = combineFileJarList();
        
        m_Submenu = submenu;
        setMenu();
        // initialize output from ITK library to go to console. 
        // Doesn't work, posted to itk-users, 2/25/08
        //if (ms_OutputWindow == null) {
        //    ms_OutputWindow = new ItkConsoleOutput();
        //    itkOutputWindow.SetInstance(ms_OutputWindow);
        //}
    }

    // invoke a config dialog or filter dialog, via menu item. Eclipse wants @Overide on this....
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
            String data_type = "F2F2";
            FilterRecord fr = matchListName(m_FilterList, name);
            ModelImage model_image = m_Frame.getActiveImage();
            int pixel_type = model_image.getType();
            String pixel_type_str = MODELIMAGE_TYPE_TABLE[pixel_type];	
            int model_dims = model_image.getNDims();
            data_type = pixel_type_str + model_dims + pixel_type_str + model_dims;
            if (fr != null && !fr.m_IOType.isEmpty()) {
                //data_type = fr.m_IOType.get(0);
            }
                    
            
                
            Class<?> cls = null;
            try {
                cls = Class.forName("InsightToolkit.itk" + name + "Filter" + data_type);
                //msg = "Found "+ cls.getName() + "\n";
            }
            catch (ClassNotFoundException cnfe) {
                // TODO user friendly available types.
                msg = "The filter " + name + " does not accept input data of type: " + 
                    ModelImage.getBufferTypeStr(pixel_type) + " " + model_dims + "D.\n"
                    + "Please convert the image first to one of these types: \n" +
                    fr.m_IOType;
                JOptionPane.showMessageDialog(m_Frame,
                                              msg, "Filter class input mismatch",
                                              JOptionPane.PLAIN_MESSAGE);
                return;
            }
            // Create a filter object, matching input image type.
            Object filter_pointer_obj = createFilterObj(cls);
            if (filter_pointer_obj == null) return;
            Object filter_obj = invokeMethod("GetPointer", filter_pointer_obj);

            itkImageF2 itk_input = null;
            if (data_type.equals("F2F2")) {
                itk_input = InsightToolkitSupport.itkCreateImageSingle2D(model_image);
                if (itk_input != null) {
                    invokeMethod("SetInput", filter_obj, null, itk_input);
                }
            }

            boolean do_set = ItkFilterRunDialog.showDialog(
                                        m_Frame,
                                        m_Frame,	
                                        "Set parameters for filter:",	
                                        name + " Filter",
                                        filter_obj);
            System.out.println("Do set " + do_set);
            if (do_set && itk_input != null) {
                // execute filter
                invokeMethod("Update", filter_obj);
                itkImageF2 itk_output = (itkImageF2)invokeMethod("GetOutput", filter_obj);
                if (itk_output != null) {
                    //System.out.println("Success, maybe: " + itk_output.GetImageDimension());
                    ModelImage model_image_result = (ModelImage) model_image.clone();
                    model_image_result.setImageName(model_image.getImageName() + "_itkfilter");

                    //copy itk result into model image.
                    InsightToolkitSupport.itkTransferImageSingle2D(null, itk_output, null, model_image_result);

                    // show the results...
                    new ViewJFrameImage(model_image_result, null, new Dimension(610, 200));
                }
            }
        }
    }

    /** 
     * Produce a list of the 'Set' methods of a filter class,
     * if this is a sub-class of an ImageToImage filter.
     * @param cls
     * @return list of Set method objects, or null if not useful.
     */
    public static List<Method> findSetMethods(Class<?> cls) {
        // if this IS the ImageToImage class, we don't want to use it.
        if (cls.getSimpleName().contains("ImageToImage")) return null;
        //String out = "";
        List<Method> out_list = new ArrayList<Method>();
        boolean found_one = false;
        boolean found_i2i_base = false;
        for (; cls != null; cls = cls.getSuperclass()) {
            // ImageToImageFilter seems to be a general base class for filters.
            // However, there is a different one for each input/output type.
            if (cls.getSimpleName().contains("ImageToImage")) {
                found_i2i_base = true;
                break;
            }
            //out += "   " + cls.getSimpleName() + "\n";
            for (Method mthd : cls.getDeclaredMethods()) {
                if (mthd.getName().startsWith("Set")) {
                    found_one = true;
                    out_list.add(mthd);
                }
            }
        }
        return (found_i2i_base ? out_list : null);
    }

    /** Determine whether this cls is probably a valid ITK image to image
     * filter which might be useful. 
     * @param cls the filter type
     * @return true if it's probably useful.
     */
    private static boolean validFilterClass(Class<?> cls) 
    {
        if (cls.getSimpleName().contains("ImageToImage")) return false;
        // Check if this is a base class for other filters - 
        // if so, it will have no factory method - no static "_New" method.
        try {
            cls.getMethod(cls.getSimpleName() + "_New", (Class<?>[])null);
        }
        catch (NoSuchMethodException nsme) {
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
        if (!found_i2i_base) return false;
        return true;
    }

    /**
     * Sets the current list of filters in our menu
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
     * Generate all menu items for filters and filter config dialog.
     * @return list of menu items, at least contains the config command.
     */
    JComponent[] getMenuItems()
    {
        if (m_FilterList == null) {
            m_FilterList = combineFileJarList();
        }
        List<JComponent> item_list = new ArrayList<JComponent>();
        JMenuItem item = null;
        JMenu more_menu = null;
        int added_count = 0;
        if (m_FilterList != null) {
            for(Iterator<FilterRecord> it = m_FilterList.iterator(); it.hasNext(); ) {
                FilterRecord filter_rec = it.next();
                if (filter_rec.m_Active && filter_rec.m_State != FilterRecord.FilterState.REMOVED) {
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
	
    /** combine information from the current jar, and the saved records with
     * filters turned on/off, into a single list reflecting active filters and 
     * NEW, NORMAL, REMOVED state.
     * @return list, or null if jar _and_ serialization file can't be read. 
     */
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
     * Get the InsightToolkit.jar from the classpath, and search it for Filter
     * classes.
     * @return FilterRecords stored in the jar, all active.
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
        //String last_filter_str = "";
        FilterRecord last_filter = null;
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
            // ignore _Pointer classes, since they match another class.
            if (jef_name.lastIndexOf("_Pointer") > 0) continue;

            // only list classes that start with "itk"
            index = jef_name.indexOf("itk");
            if (index != 0) continue;
            // remove "itk" from the front.
            jef_name = jef_name.substring(3);

            // only concentrate on Filters.
            String filter_str = "Filter";
            int filter_str_len = filter_str.length();
            index = jef_name.indexOf(filter_str);
            if (index <= 0) continue;

            // contains everything before "Filter"
            String short_jef_name = jef_name.substring(0, index);

            // See if we can find a data type string.
            // If there's nothing after "Filter", go on.
            if (index + filter_str_len >= jef_name.length()) continue;
            String type_str = jef_name.substring(index + filter_str_len);
            // ignore JNI classes, those are just underlying static impl classes.
            if (type_str.indexOf("JNI") >= 0) continue;
            // ignore types with _, probably D2D2_Superclass, or similar.
            if (type_str.indexOf("_") >= 0) continue;

            // don't add a filter record until we get a type. 
            
            Class<?> cls = null;
            try {
                cls = Class.forName("InsightToolkit.itk" + short_jef_name + "Filter" + type_str);
                //System.out.println("Found " + cls.getName());
            }
            catch (ClassNotFoundException cnfe) {
                System.out.println("Jar No luck, " + jef_name);
                continue;
            }
            
            if (last_filter == null || !short_jef_name.equals(last_filter.m_Name)) {
                // test to see if this new class is a useful ImageToImage filter, 
                if (!validFilterClass(cls)) continue;
                last_filter = new FilterRecord(short_jef_name, true);
                filter_list.add(last_filter);
                count++;
            }
            // add type string.
            last_filter.m_IOType.add(type_str);

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

    /** Generate a list of FilterRecord from our default serialized file.
     * @return list, or null if file doesn't yet exist or is not readable or compatible.
     */
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
            //if (ret_list.size() == 3) {
            //    ret_list.add(new FilterRecord("RemoveTestImage", true));
            //}
        }
        return ret_list;
    }


    /** Remove all REMOVED filter records, then write the list to a file.
     * @return
     */
    private boolean writeListToFile()
    {
        for(Iterator<FilterRecord> it = m_FilterList.iterator(); it.hasNext(); ) {
            if (it.next().m_State == FilterRecord.FilterState.REMOVED) {
                it.remove();
            }
        }

        boolean ret = true;
        ObjectOutputStream out = null;
        try {
            out = new ObjectOutputStream(new
                                         BufferedOutputStream(new FileOutputStream(LIST_SERIALIZE_FILE)));

            out.writeObject(m_FilterList);
        }
        catch (IOException io_exception) {
            System.err.println("writeListToFile " + io_exception.toString());
            ret = false;
        } finally {
            try {
                if (out != null) out.close();
            } catch (IOException io_exception) { }
        }
        return ret;
    }

    /** Given a class representing an Itk filter object, create an instance. 
     * Uses itk smart pointers.
     * @param cls the filter's type.
     * @return instance of the filter class.
     */
    public static Object createFilterObj(Class<?> cls)
    {
        // Each filter class in ITK has a corresponding Pointer type,
        // so filter T is created:
        // T_Pointer = T.T_New();
        // T is passed in as the template arg to our arg cls, so we need to 
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

    /** Invoke a method on an object, without throwing exceptions. no arg method
     * @param method_name Name of the method
     * @param obj invoke this object's method
     * @return result of the method, might be null (including for void return).
     */
    public static Object invokeMethod(String method_name, Object obj) 
    {
        return invokeMethod(method_name, obj, null, (Object[])null);
    }

    // no arg method
//     public static Object invokeMethod(String method_name, Object obj, 
//                                       Class<?> cls) 
//     {
//         return invokeMethod(method_name, obj, cls, null);
//     }

    /** Invoke a method by name without throwing exceptions.
     * @param method_name 
     * @param obj If non-null, invoke this object's method. If null, static class method.
     * @param cls If obj is non-null, ignored. Otherwise the class for static method.
     * @param obj_arg Variable number of args, passed as objects.
     * @return
     */
    public static Object invokeMethod(String method_name, Object obj, 
                                      Class<?> cls, Object... obj_arg)
    {
        assert(obj != null || cls != null);

        if (cls == null) {
            cls = obj.getClass();
        }
        Method m = null;

        // Set up list of arguments. In ITK, always 0 or 1 arg.
        Class<?> class_args [] = null; 
        if (obj_arg != null && obj_arg.length > 0) {
            class_args = new Class<?>[obj_arg.length];
            for (int i = 0; i < obj_arg.length; i++) {
                class_args[i] = null;
                try {
                    // See if obj_arg passed in has a TYPE field, which means we should
                    // retrieve the primitive Class object for it from that Field.
                    Field f = obj_arg[i].getClass().getField("TYPE");
                    class_args[i] = (Class<?>)f.get(obj_arg);
                } catch (NoSuchFieldException nsfe) {
                } catch (IllegalAccessException iae) {
                }

                if (class_args[i] == null) {
                    // get normal class for arg, if not filled in above.
                    class_args[i] =obj_arg[i].getClass();
                }
            }
        }
        try {
            m = cls.getMethod(method_name, class_args);
        }
        catch (NoSuchMethodException nsme) {
            // Expect some GetElement calls to fail, because of int/long as arg.
            if (!method_name.equals("GetElement")) {
                System.out.println("No luck, " + nsme.toString());
            }
            return null;
        }
        return invokeMethod(obj, m, obj_arg);
    }

    /** Invoke an existing method on an object instance. Avoid throwing exceptions.
     * @param obj May be null for static method.
     * @param m method to invoke.
     * @param obj_arg variable list of args.
     * @return method result.
     */
    public static Object invokeMethod(Object obj, Method m, Object... obj_arg) 
    {
        Object ret_obj = null;
        try {
            // if obj_arg is null, no args. Otherwise, array of args.
            // if obj is null, static method.
            ret_obj = m.invoke(obj, obj_arg);
        } catch (IllegalAccessException iae) {
            // If this happens, call m.setAccessible(true);, then it won't
            System.out.println("Method invoke access problem, " + m.getName());
            return null;
        }
        catch (InvocationTargetException ite) {
            System.out.println("Method invoke " + m.getName() + ", threw an exception.");
            return null;
        } catch (IllegalArgumentException iae) {
        	System.out.println("Method invoke " + m.getName() + ", " + iae);
        }
        //System.out.println("Method " + m.getName() + " return " + ret_obj);
        return ret_obj;
    }

}
