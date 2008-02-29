package gov.nih.mipav.model.algorithms.itk.autoItk;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import java.lang.reflect.*;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;
import InsightToolkit.*;

/**
 * Given an Itk filter object, generate a dialog that lets the user set 
 * the available parameters for the filter, then indicate that they 
 * want to run the filter. The filter execution is performed by AutoItkLoader.
 * @author Geometric Tools
 */
public class ItkFilterRunDialog extends JDialog implements ActionListener, PropertyChangeListener{
    /** Inner class to associate 'set' methods with an input widget and a changed flag */
    class MethodArgRecord {
        public Method m_Method = null;
        public JComponent m_Component = null;
        public Object m_DefaultVal = null;
        public boolean m_Changed = false;
    }

    /**
     * Modal dialog single instance.
     */
    private static ItkFilterRunDialog dialog;
    /**
     * Modal dialog return value
     */
    private static boolean value = false;
    /**
     * Main panel that contains input widgets.
     */
    private JPanel m_ParamPanel = null;
    /**
     * Itk filter object to manipulate.
     */
    private Object m_FilterObj = null;
    /**
     * Discovered list of the filter's 'set' methods
     */
    private List<MethodArgRecord> m_MethodList = null;
    

    /**
     * Set up and show the dialog.  The first Component argument
     * determines which frame the dialog depends on; it should be
     * a component in the dialog's controlling frame. The second
     * Component argument should be null if you want the dialog
     * to come up with its left corner in the center of the screen;
     * otherwise, it should be the component on top of which the
     * dialog should appear.
     * @param frameComp
     * @param locationComp
     * @param labelText label at the top of the dialog
     * @param title title bar text
     * @param filter_obj itk filter object input
     * @return true if the user didn't Cancel or close.
     */
    public static boolean showDialog(Component frameComp,
                                    Component locationComp,
                                    String labelText,
                                    String title,
                                    Object filter_obj) {
        Frame frame = JOptionPane.getFrameForComponent(frameComp);
        dialog = new ItkFilterRunDialog(frame,
                                locationComp,
                                labelText,
                                title,
                                filter_obj);
        dialog.setVisible(true);
        return value;
    }

    /** Modal dialog return value.
     * @param newValue
     */
    private void setValue(boolean newValue) 
    {
        value = newValue;
    }

    /** Constructor, mirrors the showDialog call. 
     * @param frame
     * @param locationComp
     * @param labelText
     * @param title
     * @param filter_obj
     */
    protected ItkFilterRunDialog(Frame frame,
                       Component locationComp,
                       String labelText,
                       String title,
                       Object filter_obj) 
    {
        super(frame, title, true);
        Class<?> filterClass = filter_obj.getClass();
        m_FilterObj = filter_obj;
        m_MethodList = new ArrayList<MethodArgRecord>();

        //Create and initialize the buttons.
        JButton cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(this);
        //
        final JButton runButton = new JButton("Run");
        runButton.setActionCommand("Run");
        runButton.addActionListener(this);
        getRootPane().setDefaultButton(runButton);

        // Main content - list of set methods with values.
        m_ParamPanel = new JPanel();
        m_ParamPanel.setLayout(new GridBagLayout());

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
            getMethodArgSetter(m_FilterObj, ar, type_str);

            String name = method_arr[i].getName();
            // strip "Set" off the front.
            JLabel lbl = new JLabel(name.substring(3) + "   " + type_str);
            gbc_label.gridy = i;
            m_ParamPanel.add(lbl, gbc_label);

            gbc_field.gridy = i;
            if (ar.m_Component != null) {
                m_ParamPanel.add(ar.m_Component, gbc_field);
            }

            m_MethodList.add(ar);
        }

        JScrollPane listScroller = new JScrollPane(m_ParamPanel);
        //listScroller.setPreferredSize(new Dimension(250, 80));
        listScroller.setAlignmentX(LEFT_ALIGNMENT);

        //Create a container so that we can add a title around
        //the scroll pane.  Can't add a title directly to the
        //scroll pane because its background would be white.
        //Lay out the label and scroll pane from top to bottom.
        JPanel listPane = new JPanel();
        listPane.setLayout(new BoxLayout(listPane, BoxLayout.PAGE_AXIS));
        JLabel label = new JLabel(labelText);
        label.setLabelFor(m_ParamPanel);
        listPane.add(label);
        listPane.add(Box.createRigidArea(new Dimension(0,5)));
        listPane.add(listScroller);
        listPane.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));

        //Lay out the buttons from left to right.
        JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
        buttonPane.add(Box.createHorizontalGlue());
        buttonPane.add(cancelButton);
        buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
        buttonPane.add(runButton);

        //Put everything together, using the content pane's BorderLayout.
        Container contentPane = getContentPane();
        contentPane.add(listPane, BorderLayout.CENTER);
        contentPane.add(buttonPane, BorderLayout.PAGE_END);

        //Initialize values.
        setValue(false);
        pack();
        
    }

    /** Create widgets that can set the param's value in the filter.
     * Save the parent widget and default value in the MethodArgRecord.
     * @param obj filter
     * @param ar in-out method record, with method filled in, modified. 
     * @param type_str output a label for the user indicating the type of input desired.
     */
    private void getMethodArgSetter(Object obj, MethodArgRecord ar, StringBuffer type_str) 
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
                JFormattedTextField ftf = new JFormattedTextField();
                ftf.setColumns(10);
                // Get default value object. 
                ar.m_DefaultVal = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                if (ar.m_DefaultVal == null) ar.m_DefaultVal = new Float(0.0);
                ftf.setValue(ar.m_DefaultVal);
                ftf.addPropertyChangeListener("value", this);
                type_str.append(FLT_LBL);
                ar.m_Component = ftf;
                for (Method mthd2 : param.getMethods()) {
                   System.out.println("primitive  " + mthd2.getName());
                   for (Class<?> cls2 : mthd2.getParameterTypes() ) {
                       System.out.println("           " + cls2.getName());
                   }
                }
                return;
            } else if (param == Long.TYPE
                       || param == Integer.TYPE
                       || param == Short.TYPE) {
                JFormattedTextField ftf = new JFormattedTextField();
                ftf.setColumns(10);
                ar.m_DefaultVal = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                // provide a default of the smallest type, so it can be converted 
                // without loss to the other types.
                if (ar.m_DefaultVal == null) ar.m_DefaultVal = new Short((short)0);
                ftf.setValue(ar.m_DefaultVal);
                ftf.addPropertyChangeListener("value", this);
                type_str.append(INT_LBL);
                ar.m_Component = ftf;
                return;
            } else if (param == Character.TYPE ||
                       param == Byte.TYPE) {
                JLabel lbl = new JLabel(param.getSimpleName());
                ar.m_Component = lbl;
                return;
            } else if (param == Boolean.TYPE) {
                JPanel button_panel = new JPanel();
                boolean on_default = true;
                ar.m_DefaultVal = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                if (ar.m_DefaultVal != null) {
                    on_default = ((Boolean)ar.m_DefaultVal).booleanValue();
                }
                JRadioButton rb_on = new JRadioButton("on");
                if (on_default) rb_on.setSelected(true);
                rb_on.setActionCommand(mthd.getName());
                rb_on.addActionListener(this);
                JRadioButton rb_off = new JRadioButton("off");
                if (!on_default) rb_off.setSelected(true);
                rb_off.setActionCommand(mthd.getName());
                rb_off.addActionListener(this);

                ButtonGroup group = new ButtonGroup();
                group.add(rb_on);
                group.add(rb_off);

                button_panel.add(rb_on);
                button_panel.add(rb_off);

                //type_str = "";
                ar.m_Component = button_panel;
                return;
            } else if (param == itkImageRegion2.class) {
                JPanel ctrl_panel = new JPanel(new GridBagLayout());
                itkImageRegion2 def_value = (itkImageRegion2)AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);

                itkSize2 def_size = null;
                itkIndex2 def_index = null;
                if (def_value != null) {
                    def_size = def_value.GetSize();
                    def_index = def_value.GetIndex();
                } else {
                    def_size = new itkSize2();
                    def_size.Fill(100);
                    def_index = new itkIndex2();
                    def_index.Fill(0);
                }                   
                ar.m_DefaultVal = def_value;
                // Index piece of region. 
                addIndexSetter(ctrl_panel, gbc, def_index, 2);

                // Size piece of region. 
                gbc.gridx = 0;
                gbc.gridy = 1;
                addSizeSetter(ctrl_panel, gbc, def_size, 2);

                type_str.append(INT_LBL);
                ar.m_Component = ctrl_panel;
                return;
            } else if (param == itkSize2.class) {
                JPanel ctrl_panel = new JPanel(new GridBagLayout());
                itkSize2 def_size = (itkSize2)AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                if (def_size == null) {
                    def_size = new itkSize2();
                    def_size.Fill(100);
                }
                ar.m_DefaultVal = def_size;
                addSizeSetter(ctrl_panel, gbc, def_size, 2);
                type_str.append(INT_LBL);
                ar.m_Component = ctrl_panel;
                return;
            } else if (param == itkIndex2.class) {
                JPanel ctrl_panel = new JPanel(new GridBagLayout());
                itkIndex2 def_index = (itkIndex2)AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                if (def_index == null) {
                    def_index = new itkIndex2();
                    def_index.Fill(0);
                }
                ar.m_DefaultVal = def_index;
                addIndexSetter(ctrl_panel, gbc, def_index, 2);
                type_str.append(INT_LBL);
                ar.m_Component = ctrl_panel;
                return;
            } else if (param_name.contains("itkPoint") || 
                       param_name.contains("itkVector") ||
                       param_name.contains("itkFixedArray")) {
                // either 2 or 3 dimensions, Point, Vector always Double data type.
                // FixedArray D, B, UI data types.
                int last_char_i = param_name.length() -1;
                String dim_str = param_name.substring(last_char_i);
                String type_str1 = param_name.substring(last_char_i - 1, last_char_i);
                String type_str2 = param_name.substring(last_char_i - 2, last_char_i);
                int dim = 2;
                try {
                    dim = Integer.parseInt(dim_str);
                } catch (NumberFormatException nfe) {
                }
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
                        ar.m_Component = new JLabel("Unavailable (error)");
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
            } else if (param.getSimpleName().contains("itkBinaryBallStructuringElement")) {
                JPanel ctrl_panel = new JPanel(new GridBagLayout());
                Object def_kernel = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                Object def_val = null;
                if (def_kernel != null) {
                    def_val = AutoItkLoader.invokeMethod("Size", def_kernel);
                }
                if (def_val == null) {
                    // default radius of 1.
                    def_val = new Long(1);
                }
                ar.m_DefaultVal = def_kernel;
                gbc.gridx = 0;
                JLabel lbl = new JLabel("radius");
                ctrl_panel.add(lbl, gbc);
                JFormattedTextField ftf = new JFormattedTextField();
                ftf.setColumns(5);
                ftf.setValue(def_val);
                ftf.addPropertyChangeListener("value", this);
                gbc.gridx = 1; 
                ctrl_panel.add(ftf, gbc);

                type_str.append(INT_LBL);
                ar.m_Component = ctrl_panel;
                return;
            } else {
                //InsightToolkit.itkNodeContainerUC2, itkImageF2/D2, etc
                String pname = param.getSimpleName();
                if (pname.startsWith("SWIGTYPE")) {
                    ar.m_Component = new JLabel("Unavailable (Swig placeholder)");
                    return;
                }
                String lbl_text = (pname.length() < 30 ?
                                   pname : 
                                   pname.substring(0, 28) + "...");
                JLabel lbl = new JLabel(lbl_text);
                ar.m_Component = lbl;
                return;
            }
        }
        ar.m_Component = null;
        return;
    }

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

            JLabel lbl = new JLabel(labels[i]);
            ctrl_panel.add(lbl, gbc);
            gbc.gridx++; 
            JFormattedTextField ftf = new JFormattedTextField();
            ftf.setColumns(5);
            ftf.setValue(elem_obj);
            ftf.addPropertyChangeListener("value", this);
            ctrl_panel.add(ftf, gbc);
            gbc.gridx++; 
        }
    }

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
     * When dialog is dismissed with 'run', call all the Set methods that had their
     * input widgets changed.
     */
    private void runSetMethods() 
    {
        // Find out whether the user changed the filter params, and call
        // matching Set methods.
        for(Iterator<MethodArgRecord> it = m_MethodList.iterator(); it.hasNext(); ) {
            MethodArgRecord ar = it.next();
            if (ar.m_Changed) {
                System.out.println("Run: Changing " + ar.m_Method.getName());
                if (ar.m_Component instanceof JFormattedTextField) {
                    //Get value from tf and call set method.
                    Object val = ((JFormattedTextField)ar.m_Component).getValue();
                    AutoItkLoader.invokeMethod(m_FilterObj, ar.m_Method, val);
                } else if (ar.m_Component instanceof JCheckBox) {
                    boolean do_invoke = ((JCheckBox)ar.m_Component).isSelected();
                    if (do_invoke) {
                        AutoItkLoader.invokeMethod(m_FilterObj, ar.m_Method);
                    }
                } else if (ar.m_Component instanceof JPanel) {
                    String def_class_name = (ar.m_DefaultVal == null ? "" : ar.m_DefaultVal.getClass().getSimpleName());
                    if (ar.m_DefaultVal instanceof Boolean) {
                        // First radio button "on", says whether boolean is true.
                        JRadioButton rb = (JRadioButton)((JPanel)ar.m_Component).getComponent(0);
                        boolean on_selected = rb.isSelected();
                        AutoItkLoader.invokeMethod(m_FilterObj, ar.m_Method, on_selected);
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
                                AutoItkLoader.invokeMethod(m_FilterObj, ar.m_Method, ar.m_DefaultVal);
                                
                                //System.out.println("Run: kernel radius " + val);
                            }
                        }
                    } else if (def_class_name.startsWith("itkPoint") ||
                               def_class_name.startsWith("itkVector") ||
                               def_class_name.startsWith("itkFixedArray") ) {
                        // reset the default val, 2 or 3 dimensions.
                        // vs itkSize, only difference is 'int' vs 'long' for first SetElement arg.
                        int tf_count = 0;
                        for (Component cmp : ar.m_Component.getComponents()) {
                            if (cmp instanceof JFormattedTextField) {
                                AutoItkLoader.invokeMethod("SetElement", ar.m_DefaultVal, null, 
                                                           tf_count, 
                                                           ((JFormattedTextField)cmp).getValue());

                                tf_count++;
                            }
                        }
                        // set the param value.
                        AutoItkLoader.invokeMethod(m_FilterObj, ar.m_Method, ar.m_DefaultVal);

                    } else if (def_class_name.startsWith("itkSize") ||
                               def_class_name.startsWith("itkIndex") ) {
                        // reset the default val, 2 or 3 dimensions.
                        // vs itkPoint, only difference is 'int' vs 'long' for first SetElement arg.
                        long tf_count = 0;
                        for (Component cmp : ar.m_Component.getComponents()) {
                            if (cmp instanceof JFormattedTextField) {
                                AutoItkLoader.invokeMethod("SetElement", ar.m_DefaultVal, null, 
                                                           tf_count, 
                                                           ((JFormattedTextField)cmp).getValue());

                                tf_count++;
                            }
                        }
                        // set the param value.
                        AutoItkLoader.invokeMethod(m_FilterObj, ar.m_Method, ar.m_DefaultVal);

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

    public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();
        if ("Run".equals(cmd)) {
            System.out.println("Run: execute the filter.");
            ItkFilterRunDialog.value = true;
            if (m_FilterObj == null) {
                System.out.println("Run: Unexpected null filter object. No action taken.");
                return;
            }
            runSetMethods();
            ItkFilterRunDialog.dialog.setVisible(false);
        } else if ("Cancel".equals(cmd)){
            ItkFilterRunDialog.dialog.setVisible(false);
        } else {
            for(Iterator<MethodArgRecord> it = m_MethodList.iterator(); it.hasNext(); ) {
                MethodArgRecord ar = it.next();
                if (ar.m_Method.getName().equals(cmd)) {
                    ar.m_Changed = true;
                }
            }
        }
            
    }

    public void propertyChange(PropertyChangeEvent evt) {
        // Set the 'Changed' flag for any (formatted text field) component that changes.
        Object source = evt.getSource();
        for(Iterator<MethodArgRecord> it = m_MethodList.iterator(); it.hasNext(); ) {
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
