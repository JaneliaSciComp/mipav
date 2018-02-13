package gov.nih.mipav.model.algorithms.itk;

import java.util.List;
import java.util.Iterator;
import java.awt.event.*;
import java.awt.*;
import javax.swing.*;


public class ItkFilterSelectorDialog extends JDialog implements ActionListener {
    /**
	 * 
	 */
	private static final long serialVersionUID = 6211274967141606057L;
	/**
     * modal dialog instance
     */
    private static ItkFilterSelectorDialog dialog;
    /**
     * modal dialog return value
     */
    private static boolean value = false;
    /**
     * panel for checkboxes, turning on/off the filters.
     */
    private JPanel m_CheckBoxPanel = null;
    /**
     * List of filters, which we modify the active state of.
     */
    private List<FilterRecordItk> m_FilterRecordItk = null;
    

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
     * @param possibleValues List of filters which the user might choose to de-activate.
     */
    public static boolean showDialog(Component frameComp,
                                    Component locationComp,
                                    String labelText,
                                    String title,
                                    List<FilterRecordItk> possibleValues) {
        Frame frame = JOptionPane.getFrameForComponent(frameComp);
        dialog = new ItkFilterSelectorDialog(frame,
                                locationComp,
                                labelText,
                                title,
                                possibleValues);
        dialog.setVisible(true);
        return value;
    }

    /** Set modal dialog return value.
     * @param newValue
     */
    private void setValue(boolean newValue) {
        value = newValue;
        //list.setSelectedValue(value, true);
    }

    /** Create the dialog
     * @param frame
     * @param locationComp
     * @param labelText
     * @param title
     * @param data List of filters which the user might choose to de-activate.
     */
    ItkFilterSelectorDialog(Frame frame,
                            Component locationComp,
                            String labelText,
                            String title,
                            List<FilterRecordItk> data) {
        super(frame, title, true);
        m_FilterRecordItk = data;

        //Create and initialize the buttons.
        JButton cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(this);
        //
        final JButton setButton = new JButton("Set");
        setButton.setActionCommand("Set");
        setButton.addActionListener(this);
        getRootPane().setDefaultButton(setButton);

        // Main content - list of filters, turn on/off.
        m_CheckBoxPanel = new JPanel();
        m_CheckBoxPanel.setLayout(new BoxLayout(m_CheckBoxPanel, BoxLayout.PAGE_AXIS));
        // add checkboxes.
        for(Iterator<FilterRecordItk> it = data.iterator(); it.hasNext(); ) {
            FilterRecordItk filter_rec = it.next();
            String name = filter_rec.m_Name + "     " + filter_rec.m_State.getName();
            JCheckBox cb = new JCheckBox(name, filter_rec.m_Active);
            if (filter_rec.m_State == FilterRecordItk.FilterState.REMOVED) {
                cb.setEnabled(false);
            }
            m_CheckBoxPanel.add(cb);
        }

        JScrollPane listScroller = new JScrollPane(m_CheckBoxPanel);
        //listScroller.setPreferredSize(new Dimension(250, 80));
        listScroller.setAlignmentX(LEFT_ALIGNMENT);

        //Create a container so that we can add a title around
        //the scroll pane.  Can't add a title directly to the
        //scroll pane because its background would be white.
        //Lay out the label and scroll pane from top to bottom.
        JPanel listPane = new JPanel();
        listPane.setLayout(new BoxLayout(listPane, BoxLayout.PAGE_AXIS));
        JLabel label = new JLabel(labelText);
        label.setLabelFor(m_CheckBoxPanel);
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
        buttonPane.add(setButton);

        //Put everything together, using the content pane's BorderLayout.
        Container contentPane = getContentPane();
        contentPane.add(listPane, BorderLayout.CENTER);
        contentPane.add(buttonPane, BorderLayout.PAGE_END);

        //Initialize values.
        setValue(false);
        pack();
        
    }

    public void actionPerformed(ActionEvent e) {
        if ("Set".equals(e.getActionCommand())) {
            //System.out.println("Set: change filter file.");
            ItkFilterSelectorDialog.value = true;
            Iterator<FilterRecordItk> it = m_FilterRecordItk.iterator(); 
            for (Component cmp : m_CheckBoxPanel.getComponents() ) {
                JCheckBox cb = (JCheckBox)cmp;
                //System.out.println(cb.getText() + " " + cb.isSelected());
                FilterRecordItk fr = it.next();
                fr.m_Active = cb.isSelected();
                // New filters have now been seen and accepted.
                if (fr.m_State == FilterRecordItk.FilterState.NEW) {
                	fr.m_State = FilterRecordItk.FilterState.NORMAL;
                }
            }
        }
        ItkFilterSelectorDialog.dialog.setVisible(false);
    }

}
