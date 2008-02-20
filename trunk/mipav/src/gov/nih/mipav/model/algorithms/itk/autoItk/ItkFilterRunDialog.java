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

public class ItkFilterRunDialog extends JDialog implements ActionListener, PropertyChangeListener{
    /** Inner class to associate 'set' methods with an input widget and a changed flag */
    class MethodArgRecord {
        public Method m_Method = null;
        public JComponent m_Component = null;
        public boolean m_Changed = false;
    }

    private static ItkFilterRunDialog dialog;
    private static boolean value = false;
    private JPanel m_ParamPanel = null;
    private Object m_FilterObj = null;
    private List<MethodArgRecord> m_MethodList = null;
    

    /**
     * Set up and show the dialog.  The first Component argument
     * determines which frame the dialog depends on; it should be
     * a component in the dialog's controlling frame. The second
     * Component argument should be null if you want the dialog
     * to come up with its left corner in the center of the screen;
     * otherwise, it should be the component on top of which the
     * dialog should appear.
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

    private void setValue(boolean newValue) 
    {
        value = newValue;
        //list.setSelectedValue(value, true);
    }

    ItkFilterRunDialog(Frame frame,
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
            ar.m_Component = getMethodArgSetter(m_FilterObj, ar.m_Method, type_str);

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
    private JComponent getMethodArgSetter(Object obj, Method mthd, StringBuffer type_str) 
    {
        final String INT_LBL = "(int)";
        final String FLT_LBL = "(float)";

        Class<?>[] params = mthd.getParameterTypes();
        if (params != null) {
            // all Set methods should take 0 or 1 parameters.
            if (params.length == 0) {
                JCheckBox cb = new JCheckBox("Activate");
                cb.setActionCommand(mthd.getName());
                cb.addActionListener(this);
                // NOT selected by default
                return cb;
            }
            assert(params.length == 1);
            Class<?> param = params[0];

            // standard setup, because of Inset.
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.gridx = 0; gbc.gridy = 0; 
            //gbc.anchor = GridBagConstraints.LINE_START;
            gbc.ipadx = gbc.ipady = 5;
            //gbc.insets = new Insets(0,2,0,2);  // padding
            
            System.out.println("Param type " + param.getName());
            if (param == Double.TYPE 
                || param == Float.TYPE) {
                JFormattedTextField ftf = new JFormattedTextField();
                ftf.setColumns(10);
                // Get default value object. 
                Object def_value = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                if (def_value == null) def_value = new Double(0);
                ftf.setValue(def_value);
                ftf.addPropertyChangeListener("value", this);
                type_str.append(FLT_LBL);
                return ftf;
            } else if (param == Integer.TYPE
                       || param == Short.TYPE) {
                JFormattedTextField ftf = new JFormattedTextField();
                ftf.setColumns(10);
                Object def_value = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                if (def_value == null) def_value = new Integer(0);
                ftf.setValue(def_value);
                ftf.addPropertyChangeListener("value", this);
                type_str.append(INT_LBL);
                return ftf;
            } else if (param == Boolean.TYPE) {
                JPanel button_panel = new JPanel();
                boolean on_default = true;
                Object def_value = AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                if (def_value != null) {
                    on_default = ((Boolean)def_value).booleanValue();
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
                return button_panel;
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
                // Index piece of region. 
                addIndexSetter(ctrl_panel, gbc, def_index);

                // Size piece of region. 
                gbc.gridy = 1;
                addSizeSetter(ctrl_panel, gbc, def_size);

                type_str.append(INT_LBL);
                return ctrl_panel;
            } else if (param == itkSize2.class) {
                JPanel ctrl_panel = new JPanel(new GridBagLayout());
                itkSize2 def_size = (itkSize2)AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                if (def_size == null) {
                    def_size = new itkSize2();
                    def_size.Fill(100);
                }
                addSizeSetter(ctrl_panel, gbc, def_size);
                type_str.append(INT_LBL);
                return ctrl_panel;
            
            } else if (param == itkIndex2.class) {
                JPanel ctrl_panel = new JPanel(new GridBagLayout());
                itkIndex2 def_index = (itkIndex2)AutoItkLoader.invokeMethod("Get" + mthd.getName().substring(3), obj);
                if (def_index == null) {
                    def_index = new itkIndex2();
                    def_index.Fill(0);
                }
                addIndexSetter(ctrl_panel, gbc, def_index);
                type_str.append(INT_LBL);
                return ctrl_panel;
            } else if (param == itkPointD2.class || param == itkVectorD2.class) {
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
                        return null;
                    }
                }
                addIndexSetter(ctrl_panel, gbc, def_val);
                //for (Method mthd2 : param.getMethods()) {
                //    System.out.println("itkPointD2  " + mthd2.getName());
                //    for (Class<?> cls2 : mthd2.getParameterTypes() ) {
                //        System.out.println("           " + cls2.getName());
                //    }
                //}
                type_str.append(FLT_LBL);
                return ctrl_panel;
            }
                //InsightToolkit.itkNodeContainerUC2
        }
        return null;
    }

    private void addIndexSetter(JPanel ctrl_panel, GridBagConstraints gbc, Object def_index)
    {
        // Get default value of the correct type. Arg might be long or int.
        Object elem_obj_0 = AutoItkLoader.invokeMethod("GetElement", def_index, null, new Long(0));
        if ( elem_obj_0 == null) {
            elem_obj_0 = AutoItkLoader.invokeMethod("GetElement", def_index, null, new Integer(0));
        }
        Object elem_obj_1 = AutoItkLoader.invokeMethod("GetElement", def_index, null, new Long(1));
        if ( elem_obj_1 == null) {
            elem_obj_1 = AutoItkLoader.invokeMethod("GetElement", def_index, null, new Integer(1));
        }

        gbc.gridx = 0;
        JLabel lbl = new JLabel("x");
        ctrl_panel.add(lbl, gbc);
        JFormattedTextField ftf = new JFormattedTextField();
        ftf.setColumns(5);
        ftf.setValue(elem_obj_0);
        ftf.addPropertyChangeListener("value", this);
        gbc.gridx = 1; 
        ctrl_panel.add(ftf, gbc);

        lbl = new JLabel("y");
        gbc.gridx = 2; 
        ctrl_panel.add(lbl, gbc);
        ftf = new JFormattedTextField();
        ftf.setColumns(5);
        ftf.setValue(elem_obj_1);
        ftf.addPropertyChangeListener("value", this);
        gbc.gridx = 3;
        ctrl_panel.add(ftf, gbc);
        
    }

    private void addSizeSetter(JPanel ctrl_panel, GridBagConstraints gbc, itkSize2 def_size)
    {
        gbc.gridx = 0;
        JLabel lbl = new JLabel("w" );
        ctrl_panel.add(lbl, gbc);
        JFormattedTextField ftf = new JFormattedTextField();
        ftf.setColumns(5);
        ftf.setValue(new Long(def_size.GetElement(0)));
        ftf.addPropertyChangeListener("value", this);
        gbc.gridx = 1; 
        ctrl_panel.add(ftf, gbc);

        lbl = new JLabel("h");
        gbc.gridx = 2; 
        ctrl_panel.add(lbl, gbc);
        ftf = new JFormattedTextField();
        ftf.setColumns(5);
        ftf.setValue(new Long(def_size.GetElement(1)));
        ftf.addPropertyChangeListener("value", this);
        gbc.gridx = 3;
        ctrl_panel.add(ftf, gbc);
    }

    public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();
        if ("Run".equals(cmd)) {
            System.out.println("Run: execute the filter.");
            ItkFilterRunDialog.value = true;
            
            for(Iterator<MethodArgRecord> it = m_MethodList.iterator(); it.hasNext(); ) {
                MethodArgRecord ar = it.next();
                System.out.println(ar.m_Method.getName() + " " + ar.m_Changed);
                
            }
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
                //amount = ((Number)((JFormattedTextField)ar.m_Component).getValue()).doubleValue();
            }
        }
    }

}
