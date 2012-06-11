package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogShortcutEditor extends JDialogBase implements ActionListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6964789155088488701L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** add shortcut button. */
    private JButton addShortcut;

    /** Keystroke built from shortcut editor dialog. */
    private KeyStroke currentShortcut = null;

    /** DOCUMENT ME! */
    private ViewTableModel defaultShortcutModel;

    /** DOCUMENT ME! */
    private JTable defaultShortcutTable;

    /** master scroll pane in which to display all information. */
    private JScrollPane masterScrollPane;

    /** button for removing shortcuts. */
    private JButton removeShortcut;

    /** Box to hold table information. */
    private Box scrollingBox;

    /** DOCUMENT ME! */
    private ViewTableModel userDefinedShortcutModel;

    /** DOCUMENT ME! */
    private JTable userDefinedShortcutTable;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor...no arguments required.
     */
    public JDialogShortcutEditor() {
        super(ViewUserInterface.getReference().getMainFrame(), false);
        setTitle("Shortcuts");
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  e  ActionEvent
     */
    public void actionPerformed(ActionEvent e) {

        if (e.getActionCommand().equals("Close")) { // close
            this.setVisible(false);
        } else if (e.getActionCommand().equals("RemoveShortcut")) {
            int row;

            // go through primary table, editing highlighted (selected ) rows
            row = userDefinedShortcutTable.getSelectedRow();

            if (row != -1) {
                String command = (String) userDefinedShortcutModel.getValueAt(row, 1);

                Preferences.removeShortcut(command);
                userDefinedShortcutModel.removeRow(row);
            }
        } else if (e.getActionCommand().equals("AddShortcut")) {
            new ShortcutDialog();
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent event) {

        if ((event.getClickCount() == 2) && (event.getButton() == MouseEvent.BUTTON1)) {
            int row;

            String command = null;

            if (event.getSource().equals(userDefinedShortcutTable)) {

                row = userDefinedShortcutTable.getSelectedRow();

                if (row != -1) {
                    command = (String) userDefinedShortcutModel.getValueAt(row, 1);
                }

            } else {
                row = defaultShortcutTable.getSelectedRow();

                if (row != -1) {
                    command = (String) defaultShortcutModel.getValueAt(row, 1);
                }

            }

            if (command != null) {

                if (ViewUserInterface.getReference().getActiveImageFrame() != null) {
                    ViewUserInterface.getReference().getActiveImageFrame().actionPerformed(new ActionEvent(this, 0,
                                                                                                           command));
                } else {
                    ViewUserInterface.getReference().actionPerformed(new ActionEvent(this, 0, command));
                }
            }

        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseReleased(MouseEvent event) { }

    /**
     * Refreshes the table by loading all shortcuts from ViewUserInterface's shortcut hashtable.
     */
    public void updateTable() {

        // re-draw shortcut table
        int rows = defaultShortcutModel.getRowCount();

        for (int i = rows - 1; i >= 0; i--) {
            defaultShortcutModel.removeRow(i);
        }

        rows = userDefinedShortcutModel.getRowCount();

        for (int i = rows - 1; i >= 0; i--) {
            userDefinedShortcutModel.removeRow(i);
        }


        String key;
        KeyStroke ks = null;
        Enumeration<String> e = Preferences.getUserShortcutTable().keys();
        String trimmed;

        Vector<String> defaultVector = new Vector<String>();

        Vector<String> userDefinedVector = new Vector<String>();

        while (e.hasMoreElements()) {
            userDefinedVector.addElement(e.nextElement());
        }
        
        e = Preferences.getDefaultShortcutTable().keys();
        while (e.hasMoreElements()) {
            defaultVector.addElement(e.nextElement());
        }
        
        
        

        Collections.sort(defaultVector, new StringVecComparator());
        Collections.sort(userDefinedVector, new StringVecComparator());

        e = defaultVector.elements();

        while (e.hasMoreElements()) {

            try {
                key = (String) e.nextElement();
                ks = Preferences.getShortcut(key);
                trimmed = new String(ks.toString());

                trimmed = trimmed.replaceAll("pressed", "").replaceAll("ctrl", "Ctrl").replaceAll("shift", "Shift").replaceAll("alt",
                                                                                                                               "Alt").trim();
                appendDefaultData(trimmed, key);
            } catch (Exception ex) {
                ex.printStackTrace();
                System.err.println("error loading shortcuts from hashtable");
            }


        }

        e = userDefinedVector.elements();

        while (e.hasMoreElements()) {

            try {
                key = (String) e.nextElement();
                ks = Preferences.getShortcut(key);
                trimmed = new String(ks.toString());

                trimmed = trimmed.replaceAll("pressed", "").replaceAll("ctrl", "Ctrl").replaceAll("shift", "Shift").replaceAll("alt",
                                                                                                                               "Alt").trim();
                appendUserDefinedData(trimmed, key);
            } catch (Exception ex) {
                ex.printStackTrace();
                System.err.println("error loading shortcuts from hashtable");
            }

        }
        // this.pack();
    }

    /**
     * appends a non editable row to the end of the primary table.
     *
     * @param  name   - file info parameter (ie., dimensions, extents, &c).
     * @param  value  - value assigned to a fileinfo parameter
     */
    private void appendDefaultData(String name, String value) {

        if (value.indexOf('\n') == -1) { // \n doesn't occur in the value

            String[] rose = { name, value };

            defaultShortcutModel.addRow(rose);
        } else {
            StringTokenizer stok = new StringTokenizer(value, "\n");
            String[] values = new String[stok.countTokens()];
            int i = 0;

            while (stok.hasMoreTokens()) {
                values[i++] = stok.nextToken();
            }

            String[] rose = { name, values[0] };

            defaultShortcutModel.addRow(rose);
            i = 1;
            rose[0] = "";

            while (i < values.length) {
                rose[1] = values[i++];
                defaultShortcutModel.addRow(rose);
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  name   DOCUMENT ME!
     * @param  value  DOCUMENT ME!
     */
    private void appendUserDefinedData(String name, String value) {

        if (value.indexOf('\n') == -1) { // \n doesn't occur in the value

            String[] rose = { name, value };

            userDefinedShortcutModel.addRow(rose);
        } else {
            StringTokenizer stok = new StringTokenizer(value, "\n");
            String[] values = new String[stok.countTokens()];
            int i = 0;

            while (stok.hasMoreTokens()) {
                values[i++] = stok.nextToken();
            }

            String[] rose = { name, values[0] };

            userDefinedShortcutModel.addRow(rose);
            i = 1;
            rose[0] = "";

            while (i < values.length) {
                rose[1] = values[i++];
                userDefinedShortcutModel.addRow(rose);
            }
        }
    }

    /**
     * makes the display frame. builds the layout.
     */
    private void init() {
        String[] columnNames = { "Shortcut", "Command" };

        try {

            // for the diff tables inside the scrollpanel:
            scrollingBox = new Box(BoxLayout.Y_AXIS);

            defaultShortcutModel = new ViewTableModel();
            defaultShortcutTable = new JTable(defaultShortcutModel);
            defaultShortcutTable.addMouseListener(this);

            userDefinedShortcutModel = new ViewTableModel();
            userDefinedShortcutTable = new JTable(userDefinedShortcutModel);
            userDefinedShortcutTable.addMouseListener(this);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("ViewFileInfo reports: Out of memory!");

            return;
        } catch (IllegalArgumentException ex) {
            MipavUtil.displayError("ViewFileInfo reports: Editing table too small!" + ex);

            return;
        }

        int i;

        for (i = 0; i < 2; i++) {
            defaultShortcutModel.addColumn(columnNames[i]);
            userDefinedShortcutModel.addColumn(columnNames[i]);
        }

        defaultShortcutTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        defaultShortcutTable.setRowSelectionAllowed(false);
        defaultShortcutTable.setColumnSelectionAllowed(false);
        defaultShortcutTable.getColumn("Shortcut").setMinWidth(160);
        defaultShortcutTable.getColumn("Shortcut").setMaxWidth(500);
        defaultShortcutTable.getColumn("Command").setMinWidth(50);
        defaultShortcutTable.getColumn("Command").setMaxWidth(1000);
        defaultShortcutTable.getTableHeader().setReorderingAllowed(false);
        defaultShortcutTable.setBackground(Color.LIGHT_GRAY);
        defaultShortcutTable.setFont(MipavUtil.defaultMenuFont);
        defaultShortcutTable.setForeground(Color.BLACK);

        userDefinedShortcutTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        userDefinedShortcutTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        userDefinedShortcutTable.getColumn("Shortcut").setMinWidth(160);
        userDefinedShortcutTable.getColumn("Shortcut").setMaxWidth(500);
        userDefinedShortcutTable.getColumn("Command").setMinWidth(50);
        userDefinedShortcutTable.getColumn("Command").setMaxWidth(1000);
        userDefinedShortcutTable.getTableHeader().setReorderingAllowed(false);
        userDefinedShortcutTable.setFont(MipavUtil.defaultMenuFont);


        updateTable();

        scrollingBox.add(defaultShortcutTable.getTableHeader());
        scrollingBox.add(defaultShortcutTable);

        scrollingBox.add(userDefinedShortcutTable);

        masterScrollPane = new JScrollPane(scrollingBox, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                           JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        masterScrollPane.getVerticalScrollBar().setUnitIncrement(14);
        getContentPane().add(masterScrollPane);

        JButton close = new JButton("Close");

        close.setActionCommand("Close");
        close.addActionListener(this);

        // close.setPreferredSize( MipavUtil.defaultButtonSize );
        close.setFont(serif12B);

        addShortcut = new JButton("Add");
        addShortcut.setActionCommand("AddShortcut");
        addShortcut.addActionListener(this);

        // addShortcut.setPreferredSize( MipavUtil.defaultButtonSize );
        addShortcut.setFont(serif12B);

        removeShortcut = new JButton("Remove");
        removeShortcut.setActionCommand("RemoveShortcut");
        removeShortcut.addActionListener(this);

        // removeParam.setPreferredSize( MipavUtil.defaultButtonSize );
        removeShortcut.setFont(serif12B);

        JPanel buttonPanel = new JPanel();

        buttonPanel.add(addShortcut);
        buttonPanel.add(addShortcut);
        buttonPanel.add(removeShortcut);
        buttonPanel.add(close);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        this.setSize(new Dimension(400, 500));


        setVisible(true);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * <p>Title: Shortcut Dialog</p>
     *
     * <p>Description: simple dialog to create new shortcut based on Ctrl, Alt, and Shift + a character</p>
     *
     * <p>Copyright: Copyright (c) 2004</p>
     *
     * <p>Company:</p>
     *
     * @author   not attributable
     * @version  1.0
     */
    private class ShortcutDialog extends JDialogBase implements ItemListener {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 2044788299697426695L;

        /** DOCUMENT ME! */
        private JCheckBox altBox;

        /** DOCUMENT ME! */
        private JTextField charField;

        /** DOCUMENT ME! */
        private JLabel charLabel;

        /** DOCUMENT ME! */
        private JCheckBox ctrlBox;

        /** DOCUMENT ME! */
        private JComboBox functionBox = null;

        /** DOCUMENT ME! */
        private String[] functionStrings = new String[] {
                                               "NONE", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10",
                                               "F11", "F12"
                                           };

        /** DOCUMENT ME! */
        private JCheckBox shiftBox;


        /**
         * Creates a new ShortcutDialog object.
         */
        public ShortcutDialog() {
            super(ViewUserInterface.getReference().getMainFrame(), true);
            init();
        }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) {
            String command = e.getActionCommand();

            if (command.equals("OK")) {

                if (setVariables()) {
                    this.setVisible(false);
                    MipavUtil.displayInfo("Click on a toolbar or menu item to select the command");
                    ViewUserInterface.getReference().setShortcutRecording(true);
                    Preferences.setCurrentShortcut(currentShortcut);
                }
            } else if (command.equals("Cancel")) {
                this.setVisible(false);
            }

        }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void itemStateChanged(ItemEvent e) {

            if (e.getSource().equals(functionBox)) {

                if (functionBox.getSelectedIndex() == 0) {
                    ctrlBox.setEnabled(true);
                    shiftBox.setEnabled(true);
                    altBox.setEnabled(ctrlBox.isSelected() || altBox.isSelected());
                    charField.setEnabled(true);
                    charLabel.setEnabled(true);

                } else {
                    ctrlBox.setEnabled(false);
                    altBox.setEnabled(false);
                    shiftBox.setEnabled(false);
                    charField.setEnabled(false);
                    charLabel.setEnabled(false);
                }
            } else if (e.getSource().equals(ctrlBox) || e.getSource().equals(shiftBox)) {
                altBox.setEnabled(ctrlBox.isSelected() || shiftBox.isSelected());

                if (!altBox.isEnabled()) {
                    altBox.setSelected(false);
                }
            }
        }

        /**
         * DOCUMENT ME!
         */
        private void init() {
            ctrlBox = new JCheckBox("Ctrl");
            ctrlBox.setFont(serif12B);

            altBox = new JCheckBox("Alt");
            altBox.setFont(serif12B);
            altBox.setEnabled(false);

            shiftBox = new JCheckBox("Shift");
            shiftBox.setFont(serif12B);

            ctrlBox.addItemListener(this);
            shiftBox.addItemListener(this);

            charField = new JTextField(2);
            charField.setFont(serif12B);
            charField.setHorizontalAlignment(JTextField.CENTER);


            functionBox = new JComboBox(functionStrings);
            functionBox.setFont(serif12B);
            functionBox.addItemListener(this);

            JPanel mainPanel = new JPanel();
            mainPanel.setLayout(new GridBagLayout());

            GridBagConstraints gbc = new GridBagConstraints();
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.anchor = GridBagConstraints.EAST;
            gbc.weightx = 1;
            gbc.fill = GridBagConstraints.BOTH;
            mainPanel.add(ctrlBox, gbc);

            gbc.gridy = 1;
            mainPanel.add(altBox, gbc);

            gbc.gridy = 2;
            mainPanel.add(shiftBox, gbc);

            charLabel = new JLabel("Character:");
            charLabel.setFont(serif12B);
            gbc.gridy = 3;
            mainPanel.add(charLabel, gbc);

            gbc.gridx = 1;
            gbc.fill = GridBagConstraints.NONE;
            mainPanel.add(charField, gbc);


            JButton okayButton = new JButton("OK");
            okayButton.addActionListener(this);
            okayButton.setFont(serif12B);

            JButton cancelButton = new JButton("Cancel");
            cancelButton.addActionListener(this);
            cancelButton.setFont(serif12B);

            JPanel buttonPanel = new JPanel();

            buttonPanel.add(okayButton);
            buttonPanel.add(cancelButton);

            mainPanel.setBorder(this.buildTitledBorder(""));

            JPanel otherPanel = new JPanel(new GridBagLayout());
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.fill = GridBagConstraints.BOTH;
            otherPanel.add(functionBox, gbc);
            otherPanel.setBorder(this.buildTitledBorder("Function keys"));

            JPanel bothPanel = new JPanel(new GridBagLayout());
            bothPanel.setBorder(buildTitledBorder(""));

            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.fill = GridBagConstraints.BOTH;
            gbc.weightx = 1;
            gbc.weighty = 1;
            bothPanel.add(mainPanel, gbc);

            gbc.gridy = 1;
            bothPanel.add(otherPanel, gbc);

            getContentPane().add(bothPanel, BorderLayout.CENTER);
            getContentPane().add(buttonPanel, BorderLayout.SOUTH);

            setResizable(true);
            pack();
            setTitle("New Shortcut");
            charField.requestFocus();
            setVisible(true);

        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        private boolean setVariables() {

            if (functionBox.getSelectedIndex() == 0) {

                try {

                    if (charField.getText().length() > 1) {
                        MipavUtil.displayWarning("Enter only one character");

                        return false;
                    }

                    char c = charField.getText().charAt(0);

                    try {
                        int val = Integer.parseInt(new String("" + c));

                        if ((val > 0) && (val < 10) && ctrlBox.isSelected() && !altBox.isSelected() &&
                                !shiftBox.isSelected()) {
                            MipavUtil.displayWarning("CTRL + (1-9) is reserved for recently used images");

                            return false;
                        }
                    } catch (Exception e) {
                        // do nothing, got past
                    }

                    int mod = 0;

                    if (ctrlBox.isSelected()) {
                        mod += Event.CTRL_MASK;
                    }

                    if (altBox.isSelected()) {
                        mod += Event.ALT_MASK;
                    }

                    if (shiftBox.isSelected()) {
                        mod += Event.SHIFT_MASK;
                    }

                    currentShortcut = KeyStroke.getKeyStroke(Character.toUpperCase(c), mod, false);

                    if (Preferences.isDefaultKeyStroke(currentShortcut)) {
                        MipavUtil.displayWarning("This key combination is reserved.");

                        return false;
                    }

                    return true;

                } catch (Exception e) {
                    MipavUtil.displayWarning("Please enter a character");

                    return false;
                }
            } else {
                currentShortcut = KeyStroke.getKeyStroke(MipavUtil.functionKeys[functionBox.getSelectedIndex()], 0,
                                                         false);

                if (Preferences.isDefaultKeyStroke(currentShortcut)) {
                    MipavUtil.displayWarning("This key combination is reserved.");

                    return false;
                }
            }

            return true;
        }

    }

    /**
     * DOCUMENT ME!
     */
    private class StringVecComparator implements Comparator<String> {

        /**
         * DOCUMENT ME!
         *
         * @param   o1  DOCUMENT ME!
         * @param   o2  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int compare(String o1, String o2) {
            int i = (o1).compareTo(o2);

            if (i == 0) {
                i = (o1).compareTo(o2);
            }

            if (i == 0) {
                i = (o1).compareTo(o2);
            }

            return i;
        }

    }


}
