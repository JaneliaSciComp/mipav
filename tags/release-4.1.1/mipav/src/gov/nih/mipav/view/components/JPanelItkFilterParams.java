package gov.nih.mipav.view.components;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Enumeration;
import java.util.Vector;
import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import java.text.DecimalFormat;
import java.lang.reflect.*;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;
import InsightToolkit.*;

import gov.nih.mipav.model.algorithms.itk.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;

/**
 * Given an Itk filter object, generate a panel that lets the user set 
 * the available parameters for the filter, then indicate that they 
 * want to run the filter. The filter execution is performed by AutoItkLoader.
 * @author Geometric Tools
 */
/**
 * @author helser
 *
 */
public class JPanelItkFilterParams extends JPanel implements ActionListener, PropertyChangeListener {
    /** Inner class to associate 'set' methods with an input widget and a changed flag */
    class MethodArgRecord {
        public Method m_Method = null;
        public JComponent m_Component = null;
        public Object m_DefaultVal = null;
        public boolean m_Changed = false;
    }

    /**
     * tabbed pane to choose 2D, 2.5D or 3D filter.
     */
    private JTabbedPane m_DimensionTab = null;
    /**
     * Main panel that contains input widgets for 2D filter.
     */
    private JPanel m_ParamPanel2D = null;
    /**
     * Main panel that contains input widgets for 3D filter.
     */
    private JPanel m_ParamPanel3D = null;

    /**
     * 2D Itk filter object to manipulate.
     */
    private Object m_FilterObj2D = null;
    /**
     * 3D Itk filter object to manipulate.
     */
    private Object m_FilterObj3D = null;

    /**
     * Discovered list of the filter's 'set' methods
     */
    private List<MethodArgRecord> m_MethodList2D = null;
    /**
     * Discovered list of the filter's 'set' methods
     */
    private List<MethodArgRecord> m_MethodList3D = null;

    /**
     * Parameters to send to the script recorder, if active.
     */
    private List<Parameter> m_ParamList = null;
    
    // I'd LOVE DecimalFormat to work, but it returns Long or Double, even if
    // I feed it a Float. I _need_ the type to be maintained, when I call
    // setValue, then getValue.  Interestingly, 'null' seems to work better
    // than the default Float/Double formatter. Weird.  i.e. it switches to
    // 'E' notation if the number is too big for the field. No ',' separator,
    // either.
    /** text field format for floats and doubles */
    private static final DecimalFormat SCI_NOTATION = null; //new DecimalFormat("0.###E0");

    /** User string for empty image filter input */
    private static final String IMG_NONE = "None";

    /** Constructor, 
     * @param src_image for filter input.
     * @param filter_obj_2D for filtering 2D images or slices of 3D
     * @param filter_obj_3D for filtering 3D images
     * @param select_25D    start with 2D filter selected when 3D is available.
     */
    public JPanelItkFilterParams(ModelImage src_image, 
                                 Object filter_obj_2D, Object filter_obj_3D, boolean select_25D) 
    {
        m_FilterObj2D = filter_obj_2D;
        m_FilterObj3D = filter_obj_3D;
        m_MethodList2D = new ArrayList<MethodArgRecord>();
        m_MethodList3D = new ArrayList<MethodArgRecord>();
        m_ParamList = new ArrayList<Parameter>();

        PanelManager myPanelManager = new PanelManager(this);
        setBorder(WidgetFactory.buildTitledBorder("Parameters"));

        m_ParamPanel2D = getPanelFillMethods(src_image, m_FilterObj2D, m_MethodList2D);
        m_ParamPanel3D = getPanelFillMethods(src_image, m_FilterObj3D, m_MethodList3D);
        
        m_DimensionTab = new JTabbedPane();
        // always add 2D tab.
        m_DimensionTab.addTab( (m_ParamPanel3D != null ? "2.5D filter slices" : "2D"), m_ParamPanel2D);
        if (m_ParamPanel3D != null) {
            m_DimensionTab.addTab("3D", m_ParamPanel3D);
            if (!select_25D) {
                m_DimensionTab.setSelectedIndex(1);
            }
        }
        if (m_ParamPanel2D == null) {
            // disable if not available. Don't think this ever happens...
            m_DimensionTab.setEnabledAt(0, false);
            m_DimensionTab.setSelectedIndex(1);
        }
        myPanelManager.add(m_DimensionTab);
    }

    /**
     * @return if 2D parameter tab is on top - user wants 2D or 2.5D filter.
     */
    public boolean is2DActive() 
    {
        if (m_DimensionTab == null) return true;
        return (m_DimensionTab.getSelectedIndex() == 0);
    }

    /** Make widgets to get values for the filter's parameters. Fill in list
     * of methods and components.
     * @param src_image for filter input.
     * @param filter_obj for filtering images or slices of 3D
     * @param method_list record of methods and components to get values.
     */
    private JPanel getPanelFillMethods(ModelImage src_image, 
                                       Object filter_obj, List<MethodArgRecord> method_list)
    {
        if (filter_obj == null) return null;

        Class<?> filterClass = filter_obj.getClass();

        // Main content - list of set methods with values.
        JPanel paramPanel = new JPanel();
        paramPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc_label = new GridBagConstraints();
        gbc_label.gridx = 0;
        gbc_label.anchor = GridBagConstraints.LINE_START;
        gbc_label.ipadx = gbc_label.ipady = 5;

        GridBagConstraints gbc_field = new GridBagConstraints();
        gbc_field.gridx = 1;
        gbc_field.fill = GridBagConstraints.HORIZONTAL;
        gbc_field.weightx = 1.0;
        gbc_field.insets = new Insets(5,5,5,5);  // padding
        //gbc_field.ipadx = gbc_field.ipady = 5;
        // add label, text field for each Set method.
        Method [] method_arr = new Method[] { };
        method_arr = AutoItkLoader.findSetMethods(filterClass).toArray(method_arr);
        for(int i = 0; i < method_arr.length ; i++) {
            MethodArgRecord ar = new MethodArgRecord();
            ar.m_Method = method_arr[i];

            StringBuffer type_str = new StringBuffer("");
            // Get an appropriate widget for setting the method's value, 
            // plus note about the type of numbers accepted. 
            getMethodArgSetter(src_image, filter_obj, ar, type_str);

            // if there's no component, don't show an interface for it.
            if (ar.m_Component != null) {
                String name = method_arr[i].getName();
                // strip "Set" off the front.
                JLabel lbl = WidgetFactory.buildLabel(name.substring(3) + "   " + type_str);
                gbc_label.gridy = i;
                paramPanel.add(lbl, gbc_label);

                gbc_field.gridy = i;
                paramPanel.add(ar.m_Component, gbc_field);

                method_list.add(ar);
            } else {
                ar = null;
            }
        }
        return paramPanel;

        //JScrollPane listScroller = new JScrollPane(m_ParamPanel);
        //listScroller.setPreferredSize(new Dimension(250, 80));
        //listScroller.setAlignmentX(LEFT_ALIGNMENT);

        //Create a container so that we can add a title around
        //the scroll pane.  Can't add a title directly to the
        //scroll pane because its background would be white.
        //Lay out the label and scroll pane from top to bottom.
        //this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        //JLabel label = new JLabel(labelText);
        //label.setLabelFor(m_ParamPanel);
        //this.add(label);
        //this.add(Box.createRigidArea(new Dimension(0,5)));
        //this.add(listScroller);
        //this.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));

    }

    /** Create widgets that can set the param's value in the filter.
     * Save the parent widget and default value in the MethodArgRecord.
     * @param src_image for filter input.
     * @param obj filter
     * @param ar in-out method record, with method filled in, modified. 
     * @param type_str output a label for the user indicating the type of input desired.
     */
    private void getMethodArgSetter(ModelImage src_image, Object obj, 
                                    MethodArgRecord ar, StringBuffer type_str) 
    {
        final String INT_LBL = "(int)";
        final String FLT_LBL = "(float)";
        Method mthd = ar.m_Method;

        Class<?>[] params = mthd.getParameterTypes();
        if (params != null) {
            // all Set methods should take 0 or 1 parameters.
            if (params.length == 0) {
                JCheckBox cb = new JCheckBox("Activate");
                cb.setActionCommand(mthd.getName());
                cb.addActionListener(this);
                // NOT selected by default
                ar.m_Component = cb;
                return;
            }
            assert(params.length == 1);
            Class<?> param = params[0];

            // standard setup, because of Inset.
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.gridx = 0; gbc.gridy = 0; 
            //gbc.anchor = GridBagConstraints.LINE_START;
            gbc.ipadx = gbc.ipady = 5;
            //gbc.insets = new Insets(0,2,0,2);  // padding
            
            //System.out.println("Param type " + param.getName());
            String param_name = param.getSimpleName();
            if (param == Double.TYPE 
                || param == Float.TYPE) {
                JFormattedTextField ftf = new JFormattedTextField(SCI_NOTATION);
                ftf.setColumns(10);
                // Get default value object. 
                ar.m_DefaultVal = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                if (ar.m_DefaultVal == null || 
                    !(ar.m_DefaultVal instanceof Float || 
                      ar.m_DefaultVal instanceof Double) ) {
                    ar.m_DefaultVal = new Float(0.0);
                }
                ftf.setValue(ar.m_DefaultVal);
                ftf.addPropertyChangeListener("value", this);
                type_str.append(FLT_LBL);
                ar.m_Component = ftf;
//                 for (Method mthd2 : param.getMethods()) {
//                    System.out.println("primitive  " + mthd2.getName());
//                    for (Class<?> cls2 : mthd2.getParameterTypes() ) {
//                        System.out.println("           " + cls2.getName());
//                    }
//                 }
                return;
            } else if (param == Long.TYPE
                       || param == Integer.TYPE
                       || param == Short.TYPE) {
                JFormattedTextField ftf = new JFormattedTextField();
                ftf.setColumns(10);
                ar.m_DefaultVal = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                // provide a default of the smallest type, so it can be converted 
                // without loss to the other types.
                if (ar.m_DefaultVal == null || 
                    !(ar.m_DefaultVal instanceof Long) ||  
                    !(ar.m_DefaultVal instanceof Integer) ||  
                    !(ar.m_DefaultVal instanceof Short) ) {
                    ar.m_DefaultVal = new Short((short)0);
                }
                ftf.setValue(ar.m_DefaultVal);
                ftf.addPropertyChangeListener("value", this);
                type_str.append(INT_LBL);
                ar.m_Component = ftf;
                return;
            } else if (param == Character.TYPE ||
                       param == Byte.TYPE) {
                JLabel lbl = WidgetFactory.buildLabel(param.getSimpleName());
                ar.m_Component = lbl;
                return;
            } else if (param == Boolean.TYPE) {
                // One exception - we ignore SetInPlace(), since it is
                // redundant with Mipav image's new/replace option.  Seems it
                // would be nice to call it with 'true' to save memory, but it
                // causes a crash when using a VOI mask, because it tries to
                // read the source image. SetInPlace(true) makes the source
                // image invalid - GetPixel crashes the JRE
                if (mthd.getName().equals("SetInPlace")) {
                    //AutoItkLoader.invokeMethod(obj, mthd, true);
                    ar.m_Component = null;
                    return;
                }
                JPanel button_panel = new JPanel();
                boolean on_default = true;
                ar.m_DefaultVal = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                if (ar.m_DefaultVal != null && 
                    ar.m_DefaultVal.getClass() == Boolean.class) {
                    on_default = ((Boolean)ar.m_DefaultVal).booleanValue();
                }
                ButtonGroup group = new ButtonGroup();
                JRadioButton rb_on = WidgetFactory.buildRadioButton("on", on_default, group);
                rb_on.setActionCommand(mthd.getName());
                rb_on.addActionListener(this);
                JRadioButton rb_off = WidgetFactory.buildRadioButton("off", !on_default, group);
                rb_off.setActionCommand(mthd.getName());
                rb_off.addActionListener(this);

                button_panel.add(rb_on);
                button_panel.add(rb_off);

                //type_str = "";
                ar.m_Component = button_panel;
                return;
            } else {
                // useful for most non-primitive types.
                int last_char_i = param_name.length() -1;
                String dim_str = param_name.substring(last_char_i);
                int dim = 2;
                try {
                    dim = Integer.parseInt(dim_str);
                } catch (NumberFormatException nfe) {
                }
                if (param_name.startsWith("itkImageRegion")) {
                    JPanel ctrl_panel = new JPanel(new GridBagLayout());
                    // itkImageRegion2 or itkImageRegion3
                    itkRegion def_value = (itkRegion)AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                    if (def_value == null) return;

                    Object def_index = AutoItkLoader.invokeMethod("GetIndex", def_value);;
                    Object def_size = AutoItkLoader.invokeMethod("GetSize", def_value);

                    ar.m_DefaultVal = def_value;
                    // Index piece of region. 
                    addIndexSetter(ctrl_panel, gbc, def_index, dim);

                    // Size piece of region. 
                    gbc.gridx = 0;
                    gbc.gridy = 1;
                    addSizeSetter(ctrl_panel, gbc, def_size, dim);

                    type_str.append(INT_LBL);
                    ar.m_Component = ctrl_panel;
                    return;
                } else if (param_name.startsWith("itkSize")) {
                    JPanel ctrl_panel = new JPanel(new GridBagLayout());
                    Object def_size = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                    if (def_size == null) {
                        if (dim == 2) {
                            def_size = new itkSize2();
                        } else if (dim == 3) {
                            def_size = new itkSize3();
                        } else {
                            return;
                        }
                        AutoItkLoader.invokeMethod("Fill", def_size, null, 100);
                    }
                    ar.m_DefaultVal = def_size;
                    addSizeSetter(ctrl_panel, gbc, def_size, dim);
                    type_str.append(INT_LBL);
                    ar.m_Component = ctrl_panel;
                    return;
                } else if (param_name.startsWith("itkIndex")) {
                    JPanel ctrl_panel = new JPanel(new GridBagLayout());
                    Object def_index = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                    if (def_index == null) {
                        if (dim == 2) {
                            def_index = new itkIndex2();
                        } else if (dim == 3) {
                             def_index = new itkIndex3();
                        } else {
                            return;
                        }
                        AutoItkLoader.invokeMethod("Fill", def_index, null, 0);
                    }
                    ar.m_DefaultVal = def_index;
                    addIndexSetter(ctrl_panel, gbc, def_index, dim);
                    type_str.append(INT_LBL);
                    ar.m_Component = ctrl_panel;
                    return;
                } else if (param_name.startsWith("itkPoint") || 
                           param_name.startsWith("itkVector") ||
                           param_name.startsWith("itkFixedArray")) {
                    // either 2 or 3 dimensions, Point, Vector always Double data type.
                    // FixedArray D, B, UI data types.
                    String type_str1 = param_name.substring(last_char_i - 1, last_char_i);
                    String type_str2 = param_name.substring(last_char_i - 2, last_char_i);
                    JPanel ctrl_panel = new JPanel(new GridBagLayout());
                    Object def_val = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                    if (def_val == null) {
                        try {
                            def_val = param.newInstance();
                        } catch (InstantiationException ie) {
                            // no default avail
                        } catch (IllegalAccessException iae) {
                            // no default avail
                        }
                        if (def_val != null) {
                            // Didn't work until I retrieved primitive class Class for arg,
                            // so I needed double.class instead of (new Double).getClass()
                            AutoItkLoader.invokeMethod("Fill", def_val, null, new Double(0.0));
                        } else {
                            ar.m_Component = WidgetFactory.buildLabel("Unavailable (error)");
                            return;
                        }
                    }
                    ar.m_DefaultVal = def_val;
                    addIndexSetter(ctrl_panel, gbc, def_val, dim);
                    //for (Method mthd2 : param.getMethods()) {
                    //    System.out.println("itkPointD2  " + mthd2.getName());
                    //    for (Class<?> cls2 : mthd2.getParameterTypes() ) {
                    //        System.out.println("           " + cls2.getName());
                    //    }
                    //}
                    type_str.append(FLT_LBL);
                    ar.m_Component = ctrl_panel;
                    return;
                } else if (param_name.startsWith("itkBinaryBallStructuringElement")) {
                    JPanel ctrl_panel = new JPanel(new GridBagLayout());
                    Object def_kernel = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                    Object def_radius = null;
                    if (def_kernel != null) {
                        def_radius = AutoItkLoader.invokeMethod("Size", def_kernel);
                    }
                    if (def_radius == null) {
                        // default radius of 1.
                        def_radius = new Long(1);
                    }
                    ar.m_DefaultVal = def_kernel;
                    gbc.gridx = 0;
                    JLabel lbl = WidgetFactory.buildLabel("radius");
                    ctrl_panel.add(lbl, gbc);
                    JFormattedTextField ftf = new JFormattedTextField();
                    ftf.setColumns(5);
                    ftf.setValue(def_radius);
                    ftf.addPropertyChangeListener("value", this);
                    gbc.gridx = 1; 
                    ctrl_panel.add(ftf, gbc);

                    type_str.append(INT_LBL);
                    ar.m_Component = ctrl_panel;
                    return;
                } else if (param_name.startsWith("itkImage")) {
                    String type_str1 = param_name.substring(last_char_i - 1, last_char_i);
                    String type_str2 = param_name.substring(last_char_i - 2, last_char_i);
                    // get list of model images of dimension consistent with ours.
                    ar.m_Component = buildComboBox(src_image, dim, mthd.getName());
                    if (dim == 2) {
                        ar.m_DefaultVal = new PItkImage2(src_image.getType());
                    } else if (dim == 3) {
                        ar.m_DefaultVal = new PItkImage3(src_image.getType());
                    }
                    return;
                }
            }
                
            //InsightToolkit.itkNodeContainerUC2, itkImageF2/D2, etc
            if (param_name.startsWith("SWIGTYPE")) {
                // return null to leave out setter completely, or a label to see it.
                ar.m_Component = null;//WidgetFactory.buildLabel("Unavailable (Swig placeholder)");
                return;
            }
            String lbl_text = (param_name.length() < 30 ?
                               param_name : 
                               param_name.substring(0, 28) + "...");
            JLabel lbl = WidgetFactory.buildLabel(lbl_text);
            ar.m_Component = lbl;
            return;
        }
        ar.m_Component = null;
        return;
    }

    /**
     * Labels for index input.
     */
    private static final String [] INDEX_LABELS = new String [] { "x", "y", "z" };

    /** Set widgets for x,y,z object, with SetElement, GetElement methods
     * @param ctrl_panel add widgets here
     * @param gbc gridded with this constraint object
     * @param def_index get default values from this object, supporting GetElement
     * @param dim number of dimensions, 2 or 3.
     */
    private void addIndexSetter(JPanel ctrl_panel, GridBagConstraints gbc, Object def_index, int dim)
    {
        addElementSetter(ctrl_panel, gbc, def_index, dim, INDEX_LABELS);
    }
    
    /** Base for addIndexSetter and addSizeSetter, labels as input.
     * @param ctrl_panel
     * @param gbc
     * @param def_index
     * @param dim
     * @param labels array of 3 labels for text input widgets.
     */
    private void addElementSetter(JPanel ctrl_panel, GridBagConstraints gbc, Object def_index, 
                                  int dim, String[] labels)
    {
    	if (dim > 3) {
    		assert(false);  // more dimensions than expected. 
    		dim = 3;
    	}
    	if (dim < 1) {
    		assert(false);  // fewer dimensions than expected. 
    		dim = 1;
    	}
        for (int i = 0; i < dim; i++) {
            // Get default value of the correct type. Arg might be long or int.
            Object elem_obj = AutoItkLoader.invokeMethod("GetElement", def_index, null, new Long(i));
            if ( elem_obj == null) {
                elem_obj = AutoItkLoader.invokeMethod("GetElement", def_index, null, new Integer(i));
            }

            JLabel lbl = WidgetFactory.buildLabel(labels[i]);
            ctrl_panel.add(lbl, gbc);
            gbc.gridx++; 
            JFormattedTextField ftf = null;
            if (elem_obj.getClass() == double.class || elem_obj.getClass() == float.class) {
                ftf = new JFormattedTextField(SCI_NOTATION);
            } else {
                ftf = new JFormattedTextField();
            }
            ftf.setColumns(5);
            ftf.setValue(elem_obj);
            ftf.addPropertyChangeListener("value", this);
            ctrl_panel.add(ftf, gbc);
            gbc.gridx++; 
        }
    }

    /**
     * Label for size input.
     */
    private static final String [] SIZE_LABELS = new String [] { "w", "h", "d" };
    
    /** Set widgets for width,height,depth object, with SetElement, GetElement methods
     * @param ctrl_panel add widgets here
     * @param gbc gridded with this constraint object
     * @param def_size get default values from this object, supporting GetElement
     * @param dim number of dimensions, 2 or 3.
     */
    private void addSizeSetter(JPanel ctrl_panel, GridBagConstraints gbc, Object def_size, int dim)
    {
        addElementSetter(ctrl_panel, gbc, def_size, dim, SIZE_LABELS);
    }

    /**
     * Builds a list of images compatible with the input image. Returns
     * combobox. List must be all color or all black and white.
     *
     * @param   image  to test for compatibility.
     * @param  check_dim number of dimensions to check for equivalence.
     *
     * @return  Newly created combo box listing image names.
     */
    private JComboBox buildComboBox(ModelImage image, int check_dim, String mthd_name) {
        ViewUserInterface UI;
        ModelImage nextImage;
        boolean doAdd, foundOne = false;
        int i;

        JComboBox comboBox = new JComboBox();
        comboBox.setFont(WidgetFactory.font12);
        comboBox.setBackground(Color.white);

        UI = ViewUserInterface.getReference();

        Enumeration names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();

            if (!name.equals(image.getImageName())) {
                nextImage = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(nextImage) != null) {

                    if ((!nextImage.isColorImage()) && (check_dim == nextImage.getNDims()) &&
                        (AutoItkLoader.getItkModelImageString(nextImage.getType()) ==
                         AutoItkLoader.getItkModelImageString(image.getType()))) {
                        doAdd = true;

                        for (i = 0; i < check_dim; i++) {

                            if (image.getExtents()[i] != nextImage.getExtents()[i]) {
                                doAdd = false;
                            }
                        }

                        if (doAdd) {
                            foundOne = true;
                            comboBox.addItem(name);
                        }
                    }
                }
            }
        }
        if (!foundOne) {
            comboBox.addItem("No available images");
        } else {
            comboBox.insertItemAt(IMG_NONE, 0);
            comboBox.setSelectedIndex(0);
            comboBox.setActionCommand(mthd_name);
            comboBox.addActionListener(this);
        }            
        return comboBox;
    }

    /**
     * When dialog is dismissed with 'run', call all the Set methods that had their
     * input widgets changed.
     * @return false on error. None so far.
     */
    public boolean runSetMethods() 
    {
        Object filterObj = null;
        Iterator<MethodArgRecord> it = null;

        ViewUserInterface UI = ViewUserInterface.getReference();

        m_ParamList.clear();

        int filter_dim = 2;
        if ( !is2DActive() && m_FilterObj3D != null ) {
            filterObj = m_FilterObj3D;
            it = m_MethodList3D.iterator() ;
            filter_dim = 3;
        } else {
            filterObj = m_FilterObj2D;
            it = m_MethodList2D.iterator();
        }
        // Find out whether the user changed the filter params, and call
        // matching Set methods.
        // NOTE: ar.m_DefaultVal should be filled with value used so scripting
        // can record which methods were invoked.
        while(it.hasNext()) {
            MethodArgRecord ar = it.next();
            if (ar.m_Changed) {
                try {
                    Parameter script_param = null;
                    Preferences.debug("Run: Changing " + ar.m_Method.getName() + "\n");
                    if (ar.m_Component instanceof JFormattedTextField) {
                        //Get value from tf and call set method.
                        ar.m_DefaultVal = ((JFormattedTextField)ar.m_Component).getValue();
                    
                        AutoItkLoader.invokeMethod(filterObj, ar.m_Method, ar.m_DefaultVal);
                        script_param = ParameterFactory.newParameter(ar.m_Method.getName(), ar.m_DefaultVal);
                    } else if (ar.m_Component instanceof JCheckBox) {
                        boolean do_invoke = ((JCheckBox)ar.m_Component).isSelected();
                        if (do_invoke) {
                            AutoItkLoader.invokeMethod(filterObj, ar.m_Method);
                            script_param = ParameterFactory.newParameter(ar.m_Method.getName(), "invoke");
                        } else {
                            // unmark Changed flag - and scripting won't record it.
                            ar.m_Changed = false;
                        }
                    } else if (ar.m_Component instanceof JComboBox) {
                        //} else if (def_class_name.startsWith("PItkImage")) {
                        // ModelImage input via combo box.
                        String sel_name = (String) ((JComboBox)ar.m_Component).getSelectedItem();
                        if (!sel_name.equals(IMG_NONE)) {
                            ModelImage image_param = UI.getRegisteredImageByName(sel_name);
                            if (image_param == null) {
                                System.out.println("Run: Can't retrieve " + sel_name);
                                continue;
                            }
                            if (filter_dim == 2) {
                                PItkImage2 itk_image = InsightToolkitSupport.itkCreateImageSingle2D(image_param);
                                if (itk_image == null) {
                                    System.out.println("Run: Can't convert " + sel_name + " to itk input.");
                                    continue;
                                }
                                // set the param value.
                                AutoItkLoader.invokeMethod(filterObj, ar.m_Method, itk_image.img());
                                script_param = ParameterFactory.newImage(ar.m_Method.getName(), sel_name, false);

                            } else if (filter_dim == 3) {                         
                                PItkImage3 itk_image = InsightToolkitSupport.itkCreateImageSingle3D(image_param);
                                if (itk_image == null) {
                                    System.out.println("Run: Can't convert " + sel_name + " to itk input.");
                                    continue;
                                }
                                // set the param value.
                                AutoItkLoader.invokeMethod(filterObj, ar.m_Method, itk_image.img());
                                script_param = ParameterFactory.newImage(ar.m_Method.getName(), sel_name, false);
                            }
                        }
                    } else if (ar.m_Component instanceof JPanel) {
                        String def_class_name = (ar.m_DefaultVal == null ? "" : ar.m_DefaultVal.getClass().getSimpleName());
                        if (ar.m_DefaultVal instanceof Boolean) {
                            // First radio button "on", says whether boolean is true.
                            JRadioButton rb = (JRadioButton)((JPanel)ar.m_Component).getComponent(0);
                            boolean on_selected = rb.isSelected();
                            ar.m_DefaultVal = on_selected;
                            AutoItkLoader.invokeMethod(filterObj, ar.m_Method, on_selected);
                            script_param = ParameterFactory.newParameter(ar.m_Method.getName(), ar.m_DefaultVal);
                        } else if (def_class_name.startsWith("itkBinaryBallStructuringElement")) {
                            // compound itk object, take action based on default value we saved.
                            // get text field.
                            for (Component cmp : ar.m_Component.getComponents()) {
                                if (cmp instanceof JFormattedTextField) {
                                    Object val = ((JFormattedTextField)cmp).getValue();
                                    // call SetRadius on BallStructuringElement
                                    AutoItkLoader.invokeMethod("SetRadius", ar.m_DefaultVal, null, val);
                                    // create the kernel
                                    AutoItkLoader.invokeMethod("CreateStructuringElement", ar.m_DefaultVal);
                                    // Set the kernel
                                    AutoItkLoader.invokeMethod(filterObj, ar.m_Method, ar.m_DefaultVal);
                                
                                    script_param = ParameterFactory.newParameter(ar.m_Method.getName(), val);
                                    //System.out.println("Run: kernel radius " + val);
                                }
                            }
                        } else if (def_class_name.startsWith("itkPoint") ||
                                   def_class_name.startsWith("itkVector") ||
                                   def_class_name.startsWith("itkFixedArray") ) {
                            // reset the default val, 2 or 3 dimensions.
                            // vs itkSize, only difference is 'int' vs 'long' for first SetElement arg.
                            int tf_count = 0;
                            ArrayList<Object> param_vals = new ArrayList<Object>();
                            for (Component cmp : ar.m_Component.getComponents()) {
                                if (cmp instanceof JFormattedTextField) {
                                    AutoItkLoader.invokeMethod("SetElement", ar.m_DefaultVal, null, 
                                                               tf_count, 
                                                               ((JFormattedTextField)cmp).getValue());
                                    param_vals.add(((JFormattedTextField)cmp).getValue());
                                    tf_count++;
                                }
                            }
                            // set the param value.
                            AutoItkLoader.invokeMethod(filterObj, ar.m_Method, ar.m_DefaultVal);
                            script_param = ParameterFactory.newParameter(ar.m_Method.getName(), param_vals.toArray());
                        } else if (def_class_name.startsWith("itkSize") ||
                                   def_class_name.startsWith("itkIndex") ) {
                            // reset the default val, 2 or 3 dimensions.
                            // vs itkPoint, only difference is 'int' vs 'long' for first SetElement arg.
                            long tf_count = 0;
                            ArrayList<Object> param_vals = new ArrayList<Object>();
                            for (Component cmp : ar.m_Component.getComponents()) {
                                if (cmp instanceof JFormattedTextField) {
                                    AutoItkLoader.invokeMethod("SetElement", ar.m_DefaultVal, null, 
                                                               tf_count, 
                                                               ((JFormattedTextField)cmp).getValue());
                                    param_vals.add(((JFormattedTextField)cmp).getValue());

                                    tf_count++;
                                }
                            }
                            // set the param value.
                            AutoItkLoader.invokeMethod(filterObj, ar.m_Method, ar.m_DefaultVal);
                            script_param = ParameterFactory.newParameter(ar.m_Method.getName(), param_vals.toArray());
                        
                        } else if (def_class_name.startsWith("itkImageRegion") ) {
                            // contains an index, then a size
                            long tf_count = 0;
                            ArrayList<Object> param_vals = new ArrayList<Object>();
                            Object val_index = AutoItkLoader.invokeMethod("GetIndex", ar.m_DefaultVal);;
                            Object val_size = AutoItkLoader.invokeMethod("GetSize", ar.m_DefaultVal);

                            for (Component cmp : ar.m_Component.getComponents()) {
                                if (cmp instanceof JFormattedTextField) {
                                    AutoItkLoader.invokeMethod("SetElement", 
                                                       tf_count < filter_dim ? val_index : val_size, 
                                                               null, 
                                                               tf_count % filter_dim, 
                                                               ((JFormattedTextField)cmp).getValue());
                                    param_vals.add(((Number)((JFormattedTextField)cmp).getValue()).intValue());

                                    tf_count++;
                                }
                            }
                            // set the parts of the region
                            AutoItkLoader.invokeMethod("SetIndex", ar.m_DefaultVal, null, val_index);
                            AutoItkLoader.invokeMethod("SetSize", ar.m_DefaultVal, null, val_size);
                            // set the param value.
                            AutoItkLoader.invokeMethod(filterObj, ar.m_Method, ar.m_DefaultVal);
                            script_param = ParameterFactory.newParameter(ar.m_Method.getName(), param_vals.toArray());
                            //System.out.println("Run: called " + ar.m_Method.getName() + "  with " 
                            //                   + param_vals);
                        
                        } else {
                            System.out.println("Run: TODO Jpanel component changed, " +
                                               "not calling " + ar.m_Method.getName());
                        }
                    } else {
                        System.out.println("Run: Unknown component changed value, " +
                                           "not calling " + ar.m_Method.getName());
                    }
                    if (script_param != null) {
                        m_ParamList.add(script_param);
                    }
                } catch (ParserException pe) {
                    System.out.println("Run: script parser " + pe);   
                }
            }
        }
        return true;
    }

    /** After algorithm has run, if script is recording, add an entry for each
     * 'set' method which was called with the correct arg value.
     * 
     * @param alg_params set of parameters to modify.
     */
    public void putScriptParams(AlgorithmParameters alg_params) throws ParserException 
    {
        ViewUserInterface UI = ViewUserInterface.getReference();
        Iterator<Parameter> it = m_ParamList.iterator();
        while(it.hasNext()) {
            Parameter param = it.next();
            // separate, higher-level handling for image params.
            if (param instanceof ParameterImage || param instanceof ParameterExternalImage) {
                ModelImage image_param = UI.getRegisteredImageByName(((ParameterString)param).getValue());
                alg_params.storeImage(image_param, param.getLabel());
            } else {
                alg_params.getParams().put(param);
            }
        }
    }

    /** Call 'set' methods for the Itk filter based on values in params table.
     * 
     * @param alg_params set of parameters to read from.
     */
    public void runFromScript(AlgorithmParameters alg_params) 
    {
        ParameterTable params = alg_params.getParams();
        Object filterObj = null;
        Iterator<MethodArgRecord> it = null;
        int filter_dim = 2;
        if ( !is2DActive() && m_FilterObj3D != null ) {
            filterObj = m_FilterObj3D;
            it = m_MethodList3D.iterator() ;
            filter_dim = 3;
        } else {
            filterObj = m_FilterObj2D;
            it = m_MethodList2D.iterator();
        }
        while(it.hasNext()) {
            MethodArgRecord ar = it.next();
            Parameter param = params.getParameter(ar.m_Method.getName());
            // Same goal as runSetMethods, above - call filter's methods with stored values.
            // so keep the same structure.
            if (param != null) {
                if (ar.m_Component instanceof JFormattedTextField) {
                    Object value = AutoItkLoader.invokeMethod("getValue", param);
                    //System.out.println("call " + ar.m_Method.getName() + " with " + value);
                    AutoItkLoader.invokeMethod(filterObj, ar.m_Method, value);
                } else if (ar.m_Component instanceof JCheckBox) {
                    // if this was recorded, we invoke with no args.
                    AutoItkLoader.invokeMethod(filterObj, ar.m_Method);
                } else if (ar.m_Component instanceof JComboBox) {
                    ModelImage image_param = alg_params.retrieveImage(param.getLabel());
                    if (filter_dim == 2) {
                        PItkImage2 itk_image = InsightToolkitSupport.itkCreateImageSingle2D(image_param);
                        if (itk_image == null) {
                            //System.out.println("Run: Can't convert " + sel_name + " to itk input.");
                            continue;
                        }
                        // set the param value.
                        AutoItkLoader.invokeMethod(filterObj, ar.m_Method, itk_image.img());
                    } else if (filter_dim == 3) {                         
                        PItkImage3 itk_image = InsightToolkitSupport.itkCreateImageSingle3D(image_param);
                        if (itk_image == null) {
                            //System.out.println("Run: Can't convert " + sel_name + " to itk input.");
                            continue;
                        }
                        // set the param value.
                        AutoItkLoader.invokeMethod(filterObj, ar.m_Method, itk_image.img());
                    }
                } else if (ar.m_Component instanceof JPanel) {
                    String def_class_name = (ar.m_DefaultVal == null ? "" : ar.m_DefaultVal.getClass().getSimpleName());
                    if (ar.m_DefaultVal instanceof Boolean) {
                        Object value = AutoItkLoader.invokeMethod("getValue", param);
                        //System.out.println("call " + ar.m_Method.getName() + " with " + value);
                        AutoItkLoader.invokeMethod(filterObj, ar.m_Method, value);
                    } else if (def_class_name.startsWith("itkBinaryBallStructuringElement")) {
                        Object value = AutoItkLoader.invokeMethod("getValue", param);
                        //System.out.println("call " + ar.m_Method.getName() + " with " + value);
                        AutoItkLoader.invokeMethod("SetRadius", ar.m_DefaultVal, null, value);
                        // create the kernel
                        AutoItkLoader.invokeMethod("CreateStructuringElement", ar.m_DefaultVal);
                        // Set the kernel
                        AutoItkLoader.invokeMethod(filterObj, ar.m_Method, ar.m_DefaultVal);
                    } else if (def_class_name.startsWith("itkPoint") ||
                               def_class_name.startsWith("itkVector") ||
                               def_class_name.startsWith("itkFixedArray") ) {
                        // Param must be a list.
                        Vector<?> value_vec = ((ParameterList)param).getList();
                        for (int tf_count = 0; tf_count < value_vec.size(); tf_count++) {
                            Object value = AutoItkLoader.invokeMethod("getValue", value_vec.get(tf_count));
                            AutoItkLoader.invokeMethod("SetElement", ar.m_DefaultVal, null, 
                                                       tf_count, value );
                        }
                        AutoItkLoader.invokeMethod(filterObj, ar.m_Method, ar.m_DefaultVal);
                    } else if (def_class_name.startsWith("itkSize") ||
                               def_class_name.startsWith("itkIndex") ) {
                        // Param must be a list.
                        Vector<?> value_vec = ((ParameterList)param).getList();
                        for (long tf_count = 0; tf_count < value_vec.size(); tf_count++) {
                            Object value = AutoItkLoader.invokeMethod("getValue", value_vec.get((int)tf_count));
                            AutoItkLoader.invokeMethod("SetElement", ar.m_DefaultVal, null, 
                                                       tf_count, value );
                        }
                        AutoItkLoader.invokeMethod(filterObj, ar.m_Method, ar.m_DefaultVal);
                    } else if (def_class_name.startsWith("itkImageRegion") ) {
                        System.out.println("Run: TODO Jpanel component changed, " +
                                           "not calling " + ar.m_Method.getName());
                    } else {
                        System.out.println("Run: TODO Jpanel component changed, " +
                                           "not calling " + ar.m_Method.getName());
                    }
                } else {
                    System.out.println("Run: Unknown component changed value, " +
                                       "not calling " + ar.m_Method.getName());
                }
            }
        }
    }

    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();
        // mark a 'changed' flag for this widget.
        Iterator<MethodArgRecord> it = ( (!is2DActive() && m_FilterObj3D != null) ?
                                         m_MethodList3D.iterator() :
                                         m_MethodList2D.iterator() );
        while(it.hasNext()) {
            MethodArgRecord ar = it.next();
            if (ar.m_Method.getName().equals(cmd)) {
                ar.m_Changed = true;
            }
        }
    }

    /* (non-Javadoc)
     * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
     */
    public void propertyChange(PropertyChangeEvent evt) {
        // Set the 'Changed' flag for any (formatted text field) component that changes.
        Object source = evt.getSource();
        Iterator<MethodArgRecord> it = ( (!is2DActive() && m_FilterObj3D != null) ?
                                         m_MethodList3D.iterator() :
                                         m_MethodList2D.iterator() );
        while(it.hasNext()) {
            MethodArgRecord ar = it.next();
            // for some Itk args, a JPanel contains the widgets that trigger
            // change events.
            if (source == ar.m_Component || 
                (ar.m_Component != null && ar.m_Component.isAncestorOf((Component)source))) {
                ar.m_Changed = true;
            }
        }
    }

}
