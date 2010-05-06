package gov.nih.mipav.view;


import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.beans.*;

import java.io.*;

import java.util.Date;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * DOCUMENT ME!
 */
public class ViewFileChooserBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int TOOLTIP_DISMISS_DELAY = 2000;

    /** DOCUMENT ME! */
    private static final int TOOLTIP_INITIAL_DELAY = 300;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected JFileChooser chooser = null;

    /** DOCUMENT ME! */
    protected FileDialog dialog = null;

    /** DOCUMENT ME! */
    protected String directory;

    /** DOCUMENT ME! */
    protected String fileName;

    /** DOCUMENT ME! */
    protected File openedFile;

    /** DOCUMENT ME! */
    protected ViewUserInterface UI;


    /** DOCUMENT ME! */
    private JPanel accessoryPanel = new JPanel();

    /** DOCUMENT ME! */
    private JButton addButton;

    /** DOCUMENT ME! */
    private JButton aliasButton;

    /** DOCUMENT ME! */
    private JTextField aliasField;

    /** DOCUMENT ME! */
    private String applicationName = new String("mipav");

    /** DOCUMENT ME! */
    private JButton deleteButton;

    /** DOCUMENT ME! */
    private String initialTitle;

    /** DOCUMENT ME! */
    private JList list;

    /** DOCUMENT ME! */
    private JScrollPane listScrollPane;

    /** DOCUMENT ME! */
    private DefaultListModel model;

    /** DOCUMENT ME! */
    private JCheckBox multiBox = null;

    /** DOCUMENT ME! */
    private int originalDismissDelay;

    /** DOCUMENT ME! */
    private int originalInitialDelay;

    /** DOCUMENT ME! */
    private JPanel panel = new JPanel();

    /** DOCUMENT ME! */
    private boolean saveAs = false;

    /** DOCUMENT ME! */
    private boolean shortcutsChanged;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ViewFileChooserBase object.
     *
     * @param  UI          DOCUMENT ME!
     * @param  openDialog  DOCUMENT ME!
     * @param  isSaveAs    DOCUMENT ME!
     */
    public ViewFileChooserBase(boolean openDialog, boolean isSaveAs) {
        this.UI = ViewUserInterface.getReference();

        this.saveAs = isSaveAs;

        if (openDialog) {

            chooser = new JFileChooser();
            chooser.setMultiSelectionEnabled(true);
            chooser.setFont(MipavUtil.defaultMenuFont);

            // chooser.setAccessory(this);
            chooser.setAccessory(accessoryPanel);

            Dimension d = new Dimension(700, 400);
            chooser.setMinimumSize(d);
            chooser.setPreferredSize(d);

            updateTitle();
            setGUI();

            MipavUtil.setFonts(chooser.getComponents());

            addListeners();
            panel.setVisible(true);
            
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getDirectory() {
        return this.directory;
    }

    /**
     * Get the active file chooser.
     *
     * @return  chooser file chooser.
     */
    public JFileChooser getFileChooser() {
        return chooser;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public FileDialog getFileDialog() {
        return dialog;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getFileName() {
        return this.fileName;
    }

    /**
     * Return the file being opened.
     *
     * @return  openedFile opened file.
     */
    public File getOpenedFile() {
        return openedFile;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isMulti() {

        if (multiBox != null) {
            return multiBox.isSelected();
        } else {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  doMulti  DOCUMENT ME!
     */
    public void setMulti(boolean doMulti) {

        if (multiBox != null) {
            multiBox.setSelected(doMulti);
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void setSaveAs() {
        this.saveAs = true;
    }



    /**
     * Adds all listeners required by this accessory.
     */

    private void addListeners() {

        // Updates chooser's title
        chooser.addPropertyChangeListener(new PropertyChangeListener() {
                public void propertyChange(PropertyChangeEvent e) {
                    String propertyName = e.getPropertyName();

                    if (propertyName.equals(JFileChooser.DIRECTORY_CHANGED_PROPERTY)) {
                        updateTitle();
                    }
                }
            });

        // Saves shortcuts when the chooser is disposed
        chooser.addAncestorListener(new AncestorListener() {
                public void ancestorRemoved(AncestorEvent e) {
                    ToolTipManager.sharedInstance().setDismissDelay(originalDismissDelay);
                    ToolTipManager.sharedInstance().setInitialDelay(originalInitialDelay);

                    if (shortcutsChanged) {
                        saveShortcuts();
                    }
                }

                public void ancestorAdded(AncestorEvent e) { }

                public void ancestorMoved(AncestorEvent e) { }
            });

        // Sets chooser's current directory or file and updates the Alias field
        list.addListSelectionListener(new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent e) {
                    int selectedIndex = list.getSelectedIndex();

                    if (selectedIndex == -1) {
                        return;
                    }

                    ShortCutted shortcut = (ShortCutted) model.get(selectedIndex);
                    String alias = shortcut.getAlias();
                    String path = shortcut.getPath();
                    String color = shortcut.getColorString();

                    String aliasText = alias;

                    if (!color.equals("black")) {
                        aliasText = color + '#' + alias;
                    }

                    aliasField.setText(aliasText);

                    File file = new File(path);

                    if (file.isFile()) {
                        chooser.setSelectedFile(file);
                    } else {
                        chooser.setCurrentDirectory(file);
                        chooser.setSelectedFile(null);
                    }
                }
            });

        // Adds/deletes/edits a shortcut
        ActionListener actionListener = new ActionListener() {
            public void actionPerformed(ActionEvent ae) {
                String command = ae.getActionCommand();

                if (command.equals("Delete")) {
                    int[] index = list.getSelectedIndices();

                    if (index.length == 0) {
                        return;
                    }

                    for (int i = 1; i < index.length; i++) {

                        for (int j = i; (j > 0) && (index[j] < index[j - 1]); j--) {
                            int temp = index[j];
                            index[j] = index[j - 1];
                            index[j - 1] = temp;
                        }
                    }

                    aliasField.setText("");

                    for (int x = index.length - 1; x >= 0; x--) {
                        model.remove(index[x]);
                    }
                }

                if (command.equals("Add")) {
                    String path;
                    File file = chooser.getSelectedFile();

                    if (file != null) {
                        path = file.getAbsolutePath();
                    } else {
                        File dir = chooser.getCurrentDirectory();
                        path = dir.getAbsolutePath();
                    }

                    insertShortcut(new ShortCutted("", path, "black"));
                }

                if (command.equals("Set")) {
                    setAlias();
                }

                list.clearSelection();
                chooser.setSelectedFile(null);
                shortcutsChanged = true;
            }
        };
        addButton.addActionListener(actionListener);
        deleteButton.addActionListener(actionListener);
        aliasButton.addActionListener(actionListener);
        aliasField.addKeyListener(new KeyAdapter() {
                public void keyPressed(KeyEvent ke) {

                    if (ke.getKeyCode() == KeyEvent.VK_ENTER) {
                        setAlias();
                        shortcutsChanged = true;
                    }
                }
            });
    }

    /**
     * Creates a DefaultListModel and populates it with shortcuts read from a file in user's home directory.
     *
     * @return  DOCUMENT ME!
     */
    private DefaultListModel createModel() {
        DefaultListModel listModel = new DefaultListModel();

        try {
            String filePath = System.getProperty("user.home") + File.separator + 
            				  applicationName + File.separator +
                              "accessory.dirs";
            File file = new File(filePath);

            if (!file.exists()) {
                return listModel;
            }

            BufferedReader in = new BufferedReader(new FileReader(file));
            String buf = null;

            while ((buf = in.readLine()) != null) {

                if (buf.startsWith("//")) { // Ignores lines with comments
                    continue;
                }

                int commaIndex = buf.indexOf(",");

                if (commaIndex == -1) {
                    throw new IOException("Incorrect format of a " + file.getPath() + " file");
                }

                String colorAndAlias = buf.substring(0, commaIndex).trim();
                String alias;
                String color;
                int hashIndex = colorAndAlias.indexOf("#");

                if (hashIndex != -1) {
                    alias = colorAndAlias.substring(hashIndex + 1);
                    color = colorAndAlias.substring(0, hashIndex);
                } else {
                    alias = colorAndAlias;
                    color = "black";
                }

                String path = buf.substring(commaIndex + 1).trim();
                ShortCutted shortcut = new ShortCutted(alias, path, color);
                listModel.addElement(shortcut);
            }

            in.close();
        } catch (IOException e) {
            e.printStackTrace();

            return null;
        }

        return listModel;
    }

    /**
     * Inserts a new shortcut into the list so that list's alphabetical order is preserved.
     *
     * @param  newShortcut  DOCUMENT ME!
     */

    private void insertShortcut(ShortCutted newShortcut) {

        if (model.getSize() == 0) {
            model.addElement(newShortcut);

            return;
        }

        // Checks if newShortcut already exists
        for (int i = 0; i < model.getSize(); i++) {
            ShortCutted shortcut = (ShortCutted) model.get(i);

            if (shortcut.getPath().equalsIgnoreCase(newShortcut.getPath())) {
                return;
            }
        }

        int insertIndex = 0;
        String newName = newShortcut.getName();

        for (int i = 0; i < model.getSize(); i++) {
            ShortCutted shortcut = (ShortCutted) model.get(i);
            String name = shortcut.getName();

            if (name.compareToIgnoreCase(newName) <= 0) {
                insertIndex = i + 1;
            } else {
                break;
            }
        }

        model.insertElementAt(newShortcut, insertIndex);
    }

    /**
     * Saves the shortcuts list to a file in user's home directory.
     */
    private void saveShortcuts() {

        try {
            String filePath = System.getProperty("user.home") + File.separator + 
			  				  applicationName + File.separator +
                              "accessory.dirs";
            PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(filePath)));
            out.println("//Directory Shortcuts for " + applicationName + " [" + new Date().toString() + ']');

            for (int i = 0; i < model.size(); i++) {
                ShortCutted shortcut = (ShortCutted) model.get(i);
                String alias = shortcut.getAlias();
                String path = shortcut.getPath();
                String color = shortcut.getColorString();
                out.println(color + '#' + alias + ',' + path);
            }

            out.close();
        } catch (IOException e) {
            e.printStackTrace();

            return;
        }
    }

    /**
     * Creates/edits/deletes an alias for a shortcut.
     */
    private void setAlias() {
        int ind = list.getSelectedIndex();

        if (ind == -1) {
            list.requestFocus();

            return;
        }

        ShortCutted shortcut = (ShortCutted) model.get(ind);
        String text = aliasField.getText().trim();

        if (text.length() == 0) { // alias removed
            shortcut.setAlias("");
            shortcut.setColor("black");
            model.remove(ind);
            insertShortcut(new ShortCutted("", shortcut.getPath(), ""));

            return;
        }

        String color = "black";
        String alias = text;
        int hashIndex = text.indexOf("#");

        if (hashIndex != -1) {
            alias = text.substring(hashIndex + 1);
            color = text.substring(0, hashIndex);
        }

        shortcut.setAlias(alias);
        shortcut.setColor(color);
        aliasField.setText("");
        model.remove(ind);
        insertShortcut(new ShortCutted(alias, shortcut.getPath(), color));
    }

    /**
     * Creates GUI for this accessory.
     */
    private void setGUI() {
        PanelManager optionsPanelManager = new PanelManager("Options");

        multiBox = new JCheckBox("Open as multifile", false);
        multiBox.setEnabled(!saveAs);
        multiBox.setFont(MipavUtil.defaultMenuFont);
        optionsPanelManager.add(multiBox);

        accessoryPanel.setBorder(BorderFactory.createLineBorder(Color.black));
        accessoryPanel.setLayout(new BorderLayout());

        panel.setBorder(MipavUtil.buildTitledBorder(" Shortcuts "));
        panel.setLayout(new BorderLayout());

        model = createModel();
        list = new JList(model) {
                public String getToolTipText(MouseEvent me) {

                    if (model.size() == 0) {
                        return null;
                    }

                    Point p = me.getPoint();
                    Rectangle bounds = list.getCellBounds(model.size() - 1, model.size() - 1);
                    int lastElementBaseline = bounds.y + bounds.height;

                    // Is the mouse pointer below the last element in the list?
                    if (lastElementBaseline < p.y) {
                        return null;
                    }

                    int index = list.locationToIndex(p);

                    if (index == -1) { // for compatibility with Java 1.3 and earlier versions
                        return null;
                    }

                    ShortCutted shortcut = (ShortCutted) model.get(index);
                    String path = shortcut.getPath();

                    if (shortcut.hasAlias()) {
                        return path;
                    }

                    FontMetrics fm = list.getFontMetrics(list.getFont());
                    int textWidth = SwingUtilities.computeStringWidth(fm, path);

                    if (textWidth <= listScrollPane.getSize().width) {
                        return null;
                    }

                    return path;
                }
            };
        list.setCellRenderer(new ListCellRenderer() {
                public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                                                              boolean cellHasFocus) {
                    ShortCutted shortcut = (ShortCutted) value;
                    String name = shortcut.getDisplayName();
                    JLabel label = new JLabel(name);
                    label.setBorder(new EmptyBorder(0, 3, 0, 3));
                    label.setOpaque(true);
                    label.setFont(MipavUtil.defaultMenuFont);

                    if (!isSelected) {
                        label.setBackground(list.getBackground());
                        label.setForeground(shortcut.getColor());
                    } else {
                        label.setBackground(list.getSelectionBackground());
                        label.setForeground(list.getSelectionForeground());
                    }

                    return label;
                }
            });
        listScrollPane = new JScrollPane(list);

        originalInitialDelay = ToolTipManager.sharedInstance().getInitialDelay();
        originalDismissDelay = ToolTipManager.sharedInstance().getDismissDelay();
        ToolTipManager.sharedInstance().setDismissDelay(TOOLTIP_DISMISS_DELAY);
        ToolTipManager.sharedInstance().setInitialDelay(TOOLTIP_INITIAL_DELAY);
        ToolTipManager.sharedInstance().registerComponent(list);

        panel.add(listScrollPane, BorderLayout.CENTER);

        JPanel southPanel = new JPanel();
        southPanel.setBorder(new EmptyBorder(3, 3, 3, 3));
        southPanel.setLayout(new BoxLayout(southPanel, BoxLayout.X_AXIS));
        addButton = new JButton("Add");
        addButton.setFont(MipavUtil.defaultMenuFont);
        addButton.setToolTipText("Add the current directory/file to Shortcuts");
        deleteButton = new JButton("Delete");
        deleteButton.setFont(MipavUtil.defaultMenuFont);
        deleteButton.setToolTipText("Delete a shortcut");
        aliasButton = new JButton("Set");
        aliasButton.setFont(MipavUtil.defaultMenuFont);
        aliasButton.setToolTipText("Set an alias for a shortcut");
        southPanel.add(Box.createHorizontalGlue());
        southPanel.add(addButton);
        southPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        southPanel.add(deleteButton);
        southPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        southPanel.add(new JLabel("  Alias:"));
        southPanel.add(Box.createRigidArea(new Dimension(2, 0)));
        aliasField = new JTextField(10);
        aliasField.setFont(MipavUtil.defaultMenuFont);
        aliasField.setMaximumSize(aliasField.getPreferredSize());
        southPanel.add(aliasField);
        southPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        southPanel.add(aliasButton);
        southPanel.add(Box.createHorizontalGlue());
        panel.add(southPanel, BorderLayout.SOUTH);

        int southPanelWidth = southPanel.getPreferredSize().width;
        Dimension size = new Dimension(southPanelWidth, 0);

        // Makes sure the accessory is not resized with addition of entries
        // longer than the current accessory width.
        panel.setPreferredSize(size);
        panel.setMaximumSize(size);

        accessoryPanel.add(panel, BorderLayout.CENTER);
        accessoryPanel.add(optionsPanelManager.getPanel(), BorderLayout.SOUTH);
    }

    /**
     * Displays the current directory path in the title bar of JFileChooser.
     */
    private void updateTitle() {

        if ((initialTitle == null) && !saveAs) {
            initialTitle = chooser.getUI().getDialogTitle(chooser);
        } else if (saveAs) {
            initialTitle = "Save image as";
        }

        chooser.setDialogTitle(initialTitle + " (" + chooser.getCurrentDirectory().getPath() + ")");
    }
}
