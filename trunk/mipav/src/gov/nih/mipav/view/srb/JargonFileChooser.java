package gov.nih.mipav.view.srb;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import edu.sdsc.grid.io.srb.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.net.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;


/**
 * This JComponent is used to select file or fold of the SRB server.
 *
 * @author   Hailong Wang 04/14/2006
 * @version  1.0
 */
public class JargonFileChooser extends JComponent implements TreeSelectionListener, ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3089967755976324109L;

    /** Instruction to display only files. */
    public static final int FILES_ONLY = 0;

    /** Instruction to display only directories. */
    public static final int DIRECTORIES_ONLY = 1;

    /** Instruction to display both files and directories. */
    public static final int FILES_AND_DIRECTORIES = 2;

    /** Return value if cancel is chosen. */
    public static final int CANCEL_OPTION = 1;

    /** Return value if approve (yes, ok) is chosen. */
    public static final int APPROVE_OPTION = 0;

    /** Return value if an error occured. */
    public static final int ERROR_OPTION = -1;

    /** Set the srb usage timeout to 30 minutes. */
    private static final long SRB_TIMEOUT_MS = 1800000;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton approveButton;

    /** DOCUMENT ME! */
    private int approveButtonMnemonic = 0;

    /** DOCUMENT ME! */
    private String approveButtonText;

    /** DOCUMENT ME! */
    private String approveButtonToolTipText = null;

    /** DOCUMENT ME! */
    private String defaultApproveButtonText = "Open";

    /** DOCUMENT ME! */
    private JDialog dialog = null;

    /** DOCUMENT ME! */
    private String dialogTitle = null;

    /** DOCUMENT ME! */
    private int fileSelectionMode = FILES_ONLY;

    /** DOCUMENT ME! */
    private SRBFileSystem fileSystem = null;

    /** DOCUMENT ME! */
    private JSRBTree jsrbTree = null;

    /** DOCUMENT ME! */
    private boolean multiSelectionEnabled = false;

    /** DOCUMENT ME! */
    private JButton newFoldButton;

    /** DOCUMENT ME! */
    private int newFoldButtonMnemonic = 1;

    /** DOCUMENT ME! */
    private int returnValue;

    /** The selected file. */
    private SRBFile selectedFile;

    /** DOCUMENT ME! */
    private JTextField selectedFileField;

    /** The selected files(multi file selection is enabled). */
    private SRBFile[] selectedFiles;

    /**
     * A thread which closes the srb connection if the user does not do anything for over 30 minutes (srb seems to have
     * a shorter timeout anyway...).
     */
    private TimeoutThread srbConnectionTimeoutMonitor = createNewSrbTimeoutMonitor();

    /** DOCUMENT ME! */
    private String title = "SRB File Chooser";

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * **** Constructor.***************************
     *
     * @param   srbAccount  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */

    public JargonFileChooser(SRBAccount srbAccount) throws IOException {
        this(srbAccount, null);
    }

    /**
     * Creates a new JargonFileChooser object.
     *
     * @param   fs  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public JargonFileChooser(SRBFileSystem fs) throws IOException {
        this(fs, null);
    }

    /**
     * Creates a new JargonFileChooser object.
     *
     * @param   uri  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public JargonFileChooser(URI uri) throws IOException {

        if (uri != null) {
            SRBFile f = new SRBFile(uri);

            System.out.print("Get back from the SRBFile constructor.");

            if (f != null) {
                init(f);
            }
        }
    }

    /**
     * Creates a new JargonFileChooser object.
     *
     * @param   srbAccount  DOCUMENT ME!
     * @param   path        DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public JargonFileChooser(SRBAccount srbAccount, String path) throws IOException {
        this(new SRBFileSystem(srbAccount), path);
    }

    /**
     * Creates a new JargonFileChooser object.
     *
     * @param   fs    DOCUMENT ME!
     * @param   path  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public JargonFileChooser(SRBFileSystem fs, String path) throws IOException {

        if (fs != null) {
            fileSystem = fs;

            if ((path == null) || (path.length() == 0)) {
                path = fs.getHomeDirectory();
            }

            if ((path == null) || (path.length() == 0)) {
                path = "/home";
            }

            SRBFile f = new SRBFile(fileSystem, path);

            if (f != null) {
                init(f);
            }

        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  args  DOCUMENT ME!
     */
    public static void main(String[] args) {

        try {
            URI uri = new URI("srb://hwang.nih:NIHBIRN@ncmir-gpop.ucsd.edu:5825/home/hwang.nih");
            JargonFileChooser chooser = new JargonFileChooser(uri);
            int ret = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), null);
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {
        String actionCommand = e.getActionCommand();

        // reset the srb timeout on an action
        srbConnectionTimeoutMonitor.shutdown();
        srbConnectionTimeoutMonitor = null;
        srbConnectionTimeoutMonitor = createNewSrbTimeoutMonitor();

        if (actionCommand.equals(approveButtonText)) {
            returnValue = APPROVE_OPTION;

            /**
             * Sets the selected files/file.
             */
            String fileName = selectedFileField.getText();

            if (isMultiSelectionEnabled()) {

                if (fileName.indexOf(",") >= 0) {
                    String[] fileNames = fileName.split(",");

                    if ((fileNames == null) || (fileNames.length == 0)) {
                        return;
                    }

                    selectedFiles = new SRBFile[fileNames.length];

                    for (int i = 0; i < fileNames.length; i++) {

                        if ((fileNames[i].length() > 0) && (fileSystem != null)) {
                            selectedFiles[i] = new SRBFile(fileSystem, fileNames[i]);
                        }
                    }
                } else {

                    if ((fileName.length() > 0) && (fileSystem != null)) {
                        selectedFile = new SRBFile(fileSystem, fileName);
                        selectedFiles = new SRBFile[1];
                        selectedFiles[0] = selectedFile;
                    }
                }
            } else {

                if ((fileName.length() > 0) && (fileSystem != null)) {
                    selectedFile = new SRBFile(fileSystem, fileName);
                    selectedFiles = new SRBFile[1];
                    selectedFiles[0] = selectedFile;
                }
            }

            if (dialog != null) {
                dialog.setVisible(false);
            }
        } else if (actionCommand.equals("newSRBFold")) {
            Object selectedObj = jsrbTree.getLastSelectedPathComponent();

            if (selectedObj == null) {
                MipavUtil.displayError("You have to select a collection before you can create a new fold.");

                return;
            }

            if (selectedObj instanceof SRBFile) {
                SRBFile srbParentFile = (SRBFile) selectedObj;

                if (srbParentFile.isFile()) {
                    srbParentFile = (SRBFile) srbParentFile.getParentFile();
                }

                new JDialogNewSRBFold("Enter new fold name", srbParentFile);
            } else {
                MipavUtil.displayError("Please select a collection.");

                return;
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getApproveButtonText() {
        return approveButtonText;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getApproveButtonToolTipText() {
        return approveButtonToolTipText;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getDialogTitle() {
        return dialogTitle;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getFileSelectionMode() {
        return fileSelectionMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public SRBFile getSelectedFile() {
        return selectedFile;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public SRBFile[] getSelectedFiles() {
        return selectedFiles;
    }

    /**
     * Returns true if multiple files can be selected.
     *
     * @return  true if multiple files can be selected
     *
     * @see     #setMultiSelectionEnabled
     */
    public boolean isMultiSelectionEnabled() {
        return multiSelectionEnabled;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  newApproveButtonText  DOCUMENT ME!
     */
    public void setApproveButtonText(String newApproveButtonText) {
        this.approveButtonText = newApproveButtonText;
        approveButton.setText(newApproveButtonText);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  toolTipText  DOCUMENT ME!
     */
    public void setApproveButtonToolTipText(String toolTipText) {

        if (approveButtonToolTipText.equals(toolTipText)) {
            return;
        }

        this.approveButtonToolTipText = toolTipText;
    }

    /**
     * Sets the title of the file chooser.
     *
     * @param  newDialogTitle  DOCUMENT ME!
     */
    public void setDialogTitle(String newDialogTitle) {

        if (getDialogTitle().equals(newDialogTitle)) {
            return;
        }

        this.dialogTitle = newDialogTitle;
        dialog.setTitle(this.dialogTitle);
        dialog.validate();
    }

    /**
     * DOCUMENT ME!
     *
     * @param   mode  DOCUMENT ME!
     *
     * @throws  IllegalArgumentException  DOCUMENT ME!
     */
    public void setFileSelectionMode(int mode) {

        if (fileSelectionMode == mode) {
            return;
        }

        if ((mode == FILES_ONLY) || (mode == DIRECTORIES_ONLY) || (mode == FILES_AND_DIRECTORIES)) {
            fileSelectionMode = mode;
        } else {
            throw new IllegalArgumentException("Incorrect Mode for file selection: " + mode);
        }
    }

    /**
     * Sets the file chooser to allow multiple file selections.
     *
     * @param  b  true if multiple files may be selected.
     */
    public void setMultiSelectionEnabled(boolean b) {

        if (multiSelectionEnabled != b) {
            multiSelectionEnabled = b;
        }

        if (multiSelectionEnabled) {
            jsrbTree.getSelectionModel().setSelectionMode(DefaultTreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
        } else {
            jsrbTree.getSelectionModel().setSelectionMode(DefaultTreeSelectionModel.SINGLE_TREE_SELECTION);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parent             DOCUMENT ME!
     * @param   approveButtonText  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int showDialog(Component parent, String approveButtonText) {

        if ((approveButtonText == null) || (approveButtonText.length() == 0)) {
            approveButtonText = defaultApproveButtonText;
        }

        setApproveButtonText(approveButtonText);
        dialog = createDialog(parent);
        dialog.addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent e) {
                    returnValue = CANCEL_OPTION;
                }
            });
        returnValue = ERROR_OPTION;

        dialog.setVisible(true);
        dialog.dispose();
        dialog = null;

        return returnValue;
    }

    /**
     * Callback method called by the SRB timeout monitor thread when the connection should be timed-out.
     */
    public void srbTimeoutMonitorCallback() {
        MipavUtil.displayError("Your SRB session has timed out.  Please log back into the SRB server.");
        srbConnectionTimeoutMonitor.shutdown();
        srbConnectionTimeoutMonitor = null;

        if (dialog != null) {
            dialog.setVisible(false);
        }

        // show error and reset the srb filesystem connection
        returnValue = ERROR_OPTION;
        JDialogLoginSRB.srbFileSystem = null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void valueChanged(TreeSelectionEvent e) {
        Object o = e.getSource();

        // reset the srb timeout on an action
        srbConnectionTimeoutMonitor.shutdown();
        srbConnectionTimeoutMonitor = null;
        srbConnectionTimeoutMonitor = createNewSrbTimeoutMonitor();

        if (o instanceof JSRBTree) {
            JSRBTree tree = (JSRBTree) o;
            TreePath[] selectedPaths = tree.getSelectionPaths();

            if ((selectedPaths == null) || (selectedPaths.length == 0)) {
                return;
            }

            StringBuffer sb = new StringBuffer("");

            if (isMultiSelectionEnabled()) {
                selectedFileField.setText("");

                for (int i = 0; i < selectedPaths.length; i++) {
                    convertSelectedPathToString(selectedPaths[i], getFileSelectionMode(), sb);
                }
            } else {
                convertSelectedPathToString(selectedPaths[selectedPaths.length - 1], getFileSelectionMode(), sb);
            }

            selectedFileField.setText(sb.toString());
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   parent  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  HeadlessException  DOCUMENT ME!
     */
    protected JDialog createDialog(Component parent) throws HeadlessException {
        JDialog dialog = null;

        if (parent instanceof Frame) {
            dialog = new JDialog((Frame) parent, title, true);
        } else if (parent instanceof Dialog) {
            dialog = new JDialog((Dialog) parent, title, true);
        }

        dialog.setComponentOrientation(this.getComponentOrientation());

        Container contentPane = dialog.getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(this, BorderLayout.CENTER);

        if (JDialog.isDefaultLookAndFeelDecorated()) {
            boolean supportsWindowDecorations = UIManager.getLookAndFeel().getSupportsWindowDecorations();

            if (supportsWindowDecorations) {
                dialog.getRootPane().setWindowDecorationStyle(JRootPane.FILE_CHOOSER_DIALOG);
            }
        }

        dialog.pack();
        dialog.setLocationRelativeTo(parent);

        return dialog;
    }

    /**
     * The helper function to set the <code>textField</code> to the selected path.
     *
     * @param   treePath       DOCUMENT ME!
     * @param   selectionMode  DOCUMENT ME!
     * @param   sb             DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private StringBuffer convertSelectedPathToString(TreePath treePath, int selectionMode, StringBuffer sb) {

        if (treePath == null) {
            return null;
        }

        if (sb == null) {
            sb = new StringBuffer("");
        }

        Object selectedObj = treePath.getLastPathComponent();

        if (selectedObj instanceof SRBFile) {
            SRBFile srbFile = (SRBFile) selectedObj;

            /*            if (selectionMode == FILES_ONLY) {
             *
             * if (srbFile.isFile()) {
             *
             * if (sb.length() == 0) {                     sb.append(srbFile.getPath());                 } else {
             * sb.append("," + srbFile.getPath());                 }             }         } else if (selectionMode ==
             * DIRECTORIES_ONLY) {
             *
             * if (srbFile.isDirectory()) {
             *
             * if (sb.length() == 0) {                     sb.append(srbFile.getPath());                 } else {
             * sb.append("," + srbFile.getPath());                 }             }         }
             * else { */

            if (sb.length() == 0) {
                sb.append(srbFile.getPath());
            } else {
                sb.append("," + srbFile.getPath());
            }
            // }
        }

        return sb;
    }

    /**
     * Creates a new SRB timeout monitor thread and attaches the file chooser as a subscriber.
     *
     * @return  A new timeout monitor thread.
     */
    private TimeoutThread createNewSrbTimeoutMonitor() {
        TimeoutThread monitor = new TimeoutThread(SRB_TIMEOUT_MS);

        try {
            monitor.addSubscriber(this, this.getClass().getMethod("srbTimeoutMonitorCallback", new Class[] {}));

            monitor.start();
        } catch (NoSuchMethodException nsme) {
            MipavUtil.displayError("Unable to start SRB timeout monitor.  Could not find callback method.");

            return null;
        }

        return monitor;
    }

    /**
     * **** Instance Functions.**********************************
     *
     * @param   f  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */

    private SRBFile getRoot(SRBFile f) {

        while (!f.getPath().equals("/home")) {
            f = (SRBFile) f.getParentFile();
        }

        return f;
    }

    /**
     * Initialize the JargonFileChooser.
     *
     * @param   f  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void init(SRBFile f) throws IOException {

        if (f == null) {
            return;
        }

        SRBFile root = getRoot(f);

        fileSystem = (SRBFileSystem) f.getFileSystem();

        jsrbTree = new JSRBTree(root.listFiles());

        jsrbTree.useDefaultPopupMenu(false);
        jsrbTree.setEditable(false);

        JScrollPane scrollPane = new JScrollPane(jsrbTree);

        scrollPane.setPreferredSize(new Dimension(600, 400));

        scrollPane.setMinimumSize(new Dimension(600, 400));

        this.setLayout(new BorderLayout(0, 11));

        PanelManager manager = new PanelManager();
        GridBagConstraints constraints = manager.getConstraints();
        manager.getConstraints().insets = new Insets(10, 3, 0, 3);

        JLabel selectedFileLabel = WidgetFactory.buildLabel("Selected Files : ");
        selectedFileLabel.setPreferredSize(new Dimension(70, 25));

        // selectedFileLabel.setFont(new Font("serif", Font.PLAIN, 14));
        manager.add(selectedFileLabel);
        selectedFileField = WidgetFactory.buildTextField("");
        selectedFileField.setPreferredSize(new Dimension(20, 25));
        selectedFileField.setColumns(40);
        manager.add(selectedFileField);
        newFoldButton = WidgetFactory.buildIconButton(UIManager.getIcon("FileChooser.newFolderIcon"),
                                                      "Create a new fold.", "newSRBFold", this);
        newFoldButton.setPreferredSize(new Dimension(32, 25));
        manager.add(newFoldButton);
        approveButton = WidgetFactory.buildTextButton(approveButtonText, "Open srb file.", approveButtonText, this);
        approveButton.setPreferredSize(new Dimension(80, 25));
        manager.add(approveButton);

        JPanel topPanel = manager.getPanel();
        topPanel.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        this.add(scrollPane, BorderLayout.CENTER);
        this.add(manager.getPanel(), BorderLayout.NORTH);
        jsrbTree.addTreeSelectionListener(this);
        jsrbTree.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0, false), "Delete");
        jsrbTree.getActionMap().put("Delete", new AbstractAction("Delete") {
                                        public void actionPerformed(ActionEvent e) {
                                            jsrbTree.delete();
                                        }
                                    });
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    private class JDialogNewSRBFold extends JDialog implements ActionListener {

        /** DOCUMENT ME! */
        private JButton cancelButton;

        /** DOCUMENT ME! */
        private JButton createButton;

        /** DOCUMENT ME! */
        private JTextField newFoldNameField;

        /** DOCUMENT ME! */
        private SRBFile parentDir;

        /**
         * Creates a new JDialogNewSRBFold object.
         *
         * @param  title   DOCUMENT ME!
         * @param  parent  DOCUMENT ME!
         */
        public JDialogNewSRBFold(String title, SRBFile parent) {
            super(dialog, title, true);
            this.parentDir = parent;
            init();
        }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) {
            String command = e.getActionCommand();

            if (command.equals("Create")) {
                create();
            } else if (command.equals("Cancel")) {
                cancel();
            }
        }

        /**
         * DOCUMENT ME!
         */
        public void init() {
            Container contentPane = this.getContentPane();
            contentPane.setLayout(new BorderLayout());

            PanelManager manager = new PanelManager();
            manager.getConstraints().insets = new Insets(10, 5, 5, 5);


            newFoldNameField = WidgetFactory.buildTextField("");
            newFoldNameField.setPreferredSize(new Dimension(20, 25));
            newFoldNameField.setColumns(40);
            manager.add(newFoldNameField);
            contentPane.add(manager.getPanel(), BorderLayout.CENTER);

            manager = new PanelManager();
            manager.getConstraints().insets = new Insets(5, 5, 10, 5);

            createButton = WidgetFactory.buildTextButton("Create", "Create new SRB fold.", "Create", this);
            createButton.setPreferredSize(new Dimension(80, 25));
            manager.addOnNextLine(createButton);
            cancelButton = WidgetFactory.buildTextButton("Cancel", "Cancel the creation of new SRB fold.", "Cancel",
                                                         this);
            cancelButton.setPreferredSize(new Dimension(80, 25));
            manager.add(cancelButton);
            contentPane.add(manager.getPanel(), BorderLayout.SOUTH);
            dialog.addWindowListener(new WindowAdapter() {
                    public void windowClosing(WindowEvent e) {
                        dialog.dispose();
                    }
                });

            // Adds the enter key response.
            newFoldNameField.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,
                                                                                                       0, false),
                                                                                "Create");
            newFoldNameField.getActionMap().put("Create", new AbstractAction("Create") {
                                                    public void actionPerformed(ActionEvent e) {
                                                        create();
                                                    }
                                                });
            cancelButton.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, false),
                                                                  "Cancel");
            cancelButton.getActionMap().put("Cancel", new AbstractAction("Cancel") {
                                                public void actionPerformed(ActionEvent e) {
                                                    cancel();
                                                }
                                            });
            newFoldNameField.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE,
                                                                                                       0, false),
                                                                                "Cancel");
            newFoldNameField.getActionMap().put("Cancel", new AbstractAction("Cancel") {
                                                    public void actionPerformed(ActionEvent e) {
                                                        cancel();
                                                    }
                                                });
            pack();

            MipavUtil.centerInComponent(JargonFileChooser.this, this);
            setVisible(true);
        }

        /**
         * DOCUMENT ME!
         */
        private void cancel() {
            this.dispose();
        }

        /**
         * DOCUMENT ME!
         */
        private void create() {
            String foldName = newFoldNameField.getText();

            if (foldName.length() > 0) {
                SRBFile newFold = new SRBFile(parentDir, foldName);
                newFold.mkdir();

                try {
                    jsrbTree.refresh(jsrbTree.getSelectionPath());
                } catch (IOException ex) {
                    MipavUtil.displayError("IOException happened: " + ex.getMessage());
                }
            }

            this.dispose();
        }
    }
}
