

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;


/**
 * Visual Interface to anonymize GE_Genesis images by the directory-full. Multiple directories may be chosen to operate on;
 * it will operate on sub-directories as well.
 *
 * <p>When one directory is chosen as the source, all anonymized Dicom files in source are placed directly in the
 * destination. Any images in any subdirectories (when chkChildren is selected) are sent into the children of the
 * sub-directories.</p>
 *
 * <p>More than one submitted source directory will be placed into discrete child directories of the submitted
 * destination directory. As as example, /images/a, /images/b are selected, and /dest is the selected destination
 * directory. They would get put into /dest/a, and /dest/b.</p>
 *
 * <p>Naming can change the way the destination is made. Selecting a top-level randomisation, will randomate the output
 * directory ofthe selected image directory. This isalong the lines in the above example as /images/a becoming
 * /dest/abcde; and /images/b becoming /dest/12345. Selecting only one input directory will notmakea difference for
 * top-level randomising. Child-directory randomising will give a random name when processing child direcories. this
 * amounts to selecting /images (and recursive processing) and the destination directory /dest,and getting /images/a
 * becoming /dest/abcde, and /images/b becoming /dest/12345.</p>
 */

public class PlugInDialogAnonymizeGE_Genesis extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID = -6606891655523582196L;

    /** DOCUMENT ME! */
    private static final int DIR_TAB = 0;

    /** DOCUMENT ME! */
    //private static final int ANON_TAB = 1;

    /** DOCUMENT ME! */
    private static final int LOG_TAB = 1;

    /** DOCUMENT ME! */
    private static final int WRITE = 0;

    /** DOCUMENT ME! */
    private static final int APPEND = 1;

    /** DOCUMENT ME! */
    private static final int OVERWRITE = 2;

    /** DOCUMENT ME! */
    public static final int NO_RANDOM_NAMES = 0;

    /** DOCUMENT ME! */
    public static final int RANDOM_DIRECTORY_NAME = 1;

    /** DOCUMENT ME! */
    public static final int RANDOM_SUBDIRECTORY_NAME = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** for the files selected ... */
    private TreePath[] actOnPath;

    /** Access to the running thread which is anonymising directories. */
    private Thread anonymizer;

    /** DOCUMENT ME! */
    private JCheckBox changeFilenameBox;

    /** DOCUMENT ME! */
    private JPanelAnonymizeImage checkBoxPanel;

    /** Actual things we can see... */
    private JTree directoryTree;

    /** Onscreen objects. */
    private JTabbedPane everything;

    /** Logical construct to reuild a source tree... */
    private JPanel filePanel; // to reset the tree, this panel and its child "sourcePanel" is needed.

    /** Used as part of the image name; used in all processed image-sets. */
    private String genericImageName; //

    /** DOCUMENT ME! */
    private ButtonGroup grouping;

    /** DOCUMENT ME! */
    private JButton imageDestBrowse;

    /** private File srcDirectory; // Defined by the. */
    private File imageDestDirectory = null;

    /** DOCUMENT ME! */
    private JTextField imageDestDirText;

    /** DOCUMENT ME! */
    private JTextField imageNameText;

    /** Translation key, stored in "patient.key.doc". */
    private String keyLog;

    /** holds output log; updated as processed. */
    private JTextArea logPane;

    /** Noisy means notifying the user of stupid errors. */
    private boolean noisyProcess = true;

    /** DOCUMENT ME! */
    private JRadioButton noRandButton;

    /** String to hold all file checks, changes and ignores. */
    private String outputLog = "";

    /** To generate wierd, unrecoverable names for anon images. */
    private Random rand = new Random();

    /** DOCUMENT ME! */
    private JRadioButton randChildButton;

    /** DOCUMENT ME! */
    private JRadioButton randTopButton;

    /** private JButton srcBrowse;. */
    private JCheckBox recursiveCheckBox;

    /** All directory-tree selected directories are listed for user-consumption in this whitespace. */
    private JTextArea selectedList;

    /** Used as part of the image name; represents beginning of sequence for all processed image sets. */
    private int sequenceStart = 0;

    /** DOCUMENT ME! */
    private JTextField sequenceText;

    /** DOCUMENT ME! */
    private JPanel sourcePanel; // remove this panel and rebuild it with a different input var to get a new directory
                                // tree.

    /** stops the thread from continuing its processing. on same panel as OK and Cancel. */
    private JButton stopButton;

    /** DOCUMENT ME! */
    private int toplevelOnly = NO_RANDOM_NAMES;

    /** DOCUMENT ME! */
    private JButton xlatDestBrowse;

    /** DOCUMENT ME! */
    private File xlatDestDirectory = null;

    /** DOCUMENT ME! */
    private JTextField xlatDestDirText;

    /** Defines options for WRITE, OVERWRITE, APPEND for the xlat file. */
    private int xlatDestinationUsage;

    
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * builds and packs the frame. does <i>not</I> set it visible.
     *
     * <p>install the panels of source directory, destination directory, the checkbox for approving the
     * translation-table file and the panel containing the ok and cancel buttons. Installs the checkbox panel.</p>
     *
     * @param  dir  DOCUMENT ME!
     */
    public PlugInDialogAnonymizeGE_Genesis () {
        super(ViewUserInterface.getReference().getMainFrame(), false);
        
     // get the selected directory
        final ViewDirectoryChooser chooser = new ViewDirectoryChooser();
        final String dir = chooser.getImageDirectory();

        setTitle("Anonymize GE_Genesis directory");
        setJMenuBar(buildMenuEntries());

        everything = new JTabbedPane(JTabbedPane.TOP);
        everything.setFont(MipavUtil.font12B);
        everything.insertTab("Directory", null, filePanel = buildFilePanel(dir), // we must store this panel so we can
                                                                                 // create a new directory listing later
                             "Choose all source and destination directories", DIR_TAB);

        JPanel anonPanel = new JPanel(new BorderLayout());
        checkBoxPanel = new JPanelAnonymizeImage();
        anonPanel.add(checkBoxPanel, BorderLayout.CENTER);
        anonPanel.add(buildNameSuggestionPanel(), BorderLayout.SOUTH);
        //everything.insertTab("Tag options", null, anonPanel, "Tag Selection", ANON_TAB);

        everything.insertTab("Logging", null, buildLogPanel(), "Process Log", LOG_TAB);

        mainDialogPanel.add(everything, BorderLayout.CENTER);
        mainDialogPanel.add(buildOKCancelPanel(), BorderLayout.SOUTH); // build OK/Cancel button Panel

        getContentPane().add(mainDialogPanel);

        pack();
        setSize(800, 500); // decent size??
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * when a button is clicked.
     *
     * @param  ae  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent ae) {
        Object source = ae.getSource();
        String command = ae.getActionCommand();

        if (command.equals("New")) {
            ViewDirectoryChooser chooser = new ViewDirectoryChooser();
            String dir = chooser.getImageDirectory();

            if (dir != null) {

                // display a new directory-tree starting at "dir"
                filePanel.remove(sourcePanel);
                filePanel.add(sourcePanel = buildSourcePanel(dir), BorderLayout.CENTER);
                validate();
            }
        } else if (command.equals("Renew")) {
            directoryTree.setSelectionRow(0); // select the root node
            filePanel.remove(sourcePanel); // remove the panel and replace it with a panel made with the name of the
                                           // initial root node.
            filePanel.add(sourcePanel = buildSourcePanel(((ViewFileTreeNode)
                                                              directoryTree.getSelectionPath().getLastPathComponent())
                                                             .getAbsolutePath()), BorderLayout.CENTER);
        } else if (command.equals("clear log")) {
            logPane.setText("");
        } else if (command.equals("Stop")) {

            if (anonymizer.isAlive()) {
                ((AnonymizeDicomDirectories) anonymizer).stopProcess();
                stopButton.setEnabled(false);
            }
        } else if ((source == OKButton) && OKButton.isEnabled()) {

            // check that there is an image path selected
            if (!isSelectedPathsOkay()) {
                return; // error notification already done.
            }

            // check that there is an destination path selected
            if (!isImageDestinationOkay()) {
                return; // error notification already done.
            }


            // check to make sure at least one tag has been selected to remove
            //if (!isRemovalSelectionOkay()) {
                //return; // error notification already done.
            //}

            // check that there is an path selected for the "patient.key.doc" translation file.
            if (!isXLATdestinationOkay()) {
                return; // error notification already done.
            }
            
            

            applyGenericName(); // convert generic name text fields into native types

            // make files out of all the selected FileNodes in the tree...
            File[] s = new File[actOnPath.length];

            for (int i = 0; i < actOnPath.length; i++) {
                s[i] = ((ViewFileTreeNode) actOnPath[i].getLastPathComponent()).getFile();
                if(s[i].getAbsolutePath().equals(imageDestDirectory.getAbsolutePath())) {
                	MipavUtil.displayError("Destination directory needs to be different that source directory");
                	return;
                }
            }

            

            keyLog = newKeyLog(); // write in selected anonymize choices, create 'new' log-table.

            if (randTopButton.isSelected()) {
                toplevelOnly = PlugInDialogAnonymizeGE_Genesis.RANDOM_DIRECTORY_NAME;
            } else if (randChildButton.isSelected()) {
                toplevelOnly = PlugInDialogAnonymizeGE_Genesis.RANDOM_SUBDIRECTORY_NAME;
            } else {
                toplevelOnly = PlugInDialogAnonymizeGE_Genesis.NO_RANDOM_NAMES;
            }

            AnonymizeDicomDirectories da = new AnonymizeDicomDirectories(s, imageDestDirectory, genericImageName,
                                                                         sequenceStart, toplevelOnly,
                                                                         changeFilenameBox.isSelected());
            da.setCheckChildren(recursiveCheckBox.isSelected());
            da.addProcessLoggingNotifier(logPane);
            da.setFinishNotification(this);

            // hold on to the reference so we can use the stop button.
            anonymizer = da;

            // notification will turn buttons back on
            everything.setSelectedIndex(LOG_TAB);
            cancelButton.setEnabled(false);
            OKButton.setEnabled(false);
            stopButton.setEnabled(true);
            da.start();

            /* although the (okay/cancel) buttons are not supposed
             * to be available to the user, the user can mouse around to view the logs, the selections and go do other
             * things in MIPAV right now.
             */
        } else if ((source == cancelButton) && cancelButton.isEnabled()) {
            dispose();
        } else if (source == imageDestBrowse) {

            // MUST use swing.  no choice for browser using prefs
            JFileChooser chuseDest = new JFileChooser(imageDestDirText.getText());
            chuseDest.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

            // chuseSrc.setFileHidingEnabled(false);   //  ????  do we want to do this?? (UNIX only?)
            chuseDest.setDialogTitle("Select destination directory");

            int returnVal = chuseDest.showDialog(this, "Select");

            if (returnVal == JFileChooser.APPROVE_OPTION) {

                // set the image/image-text-line destination
                imageDestDirectory = chuseDest.getSelectedFile();
                imageDestDirText.setText(imageDestDirectory.getAbsolutePath() + File.separator);

                // also set the file/file-text-line of the xlat destination
                xlatDestDirectory = chuseDest.getSelectedFile();
                xlatDestDirText.setText(xlatDestDirectory.getAbsolutePath() + File.separator);
            }
        } else if (source == xlatDestBrowse) {

            // MUST use swing.  no choice for browser using prefs
            JFileChooser chuseDest = new JFileChooser(imageDestDirText.getText());
            chuseDest.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

            // chuseSrc.setFileHidingEnabled(false);   //  ????  do we want to do this?? (UNIX only?)
            chuseDest.setDialogTitle("Select destination directory");

            int returnVal = chuseDest.showDialog(this, "Select");

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                xlatDestDirectory = chuseDest.getSelectedFile();
                xlatDestDirText.setText(xlatDestDirectory.getAbsolutePath() + File.separator);
            }
        }
    }

    /**
     * A psuedo-event-handler. Takes the event (An instance of AnonymizeDicomDirectories) resets the enabled status of
     * the OK and Cancel buttons, appends the key-log text to the key-log and calls writeKeyFile().
     *
     * @param  event  the event
     */
    public void anonymizationComplete(AnonymizeDicomDirectories event) {

        // notification will turn buttons back on
        cancelButton.setEnabled(true);
        OKButton.setEnabled(true);
        stopButton.setEnabled(false);
        keyLog += event.getKeyLog();
        outputLog += event.getProcessLog();
        writeKeyFile();
        System.gc(); // to reclaim lost land.
    }

    /**
     * converts the name and sequence JTextFields into native types. Ensures that if the textFields are empty, the empty
     * values don't cause problems. That means the generic name is the original generated random string and sequence is
     * 0.
     */
    private void applyGenericName() {

        if (imageNameText.getText() != "") {
            genericImageName = imageNameText.getText();
        }

        try {
            sequenceStart = Integer.parseInt(sequenceText.getText());
        } catch (NumberFormatException nfe) {

            // unable to write a notification that 0 is being assumed
            // due to error.
            // writing notification might be undesirable.
            // Is there a NICE way to do this?
            sequenceStart = 0;
        }
    }

    /**
     * creates the source panel which consists of the directory line, the browse button, and a check box approving the
     * anonymize in sub-directories.
     *
     * @param   dir  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildFilePanel(String dir) {
        JPanel imagePanel = new JPanel(new BorderLayout());

        // we must store sourcePanel so we can create a new directory listing later
        imagePanel.add(sourcePanel = buildSourcePanel(dir), BorderLayout.CENTER);

        JPanel destinationsPanel = new JPanel(new BorderLayout());
        destinationsPanel.add(buildImageDestPanel(), BorderLayout.NORTH);
        destinationsPanel.add(buildXLATdestPanel(), BorderLayout.SOUTH);

        JPanel lowerPanel = new JPanel(new BorderLayout());
        lowerPanel.add(destinationsPanel, BorderLayout.CENTER);
        lowerPanel.add(buildRandSelectionPanel(), BorderLayout.EAST);
        imagePanel.add(lowerPanel, BorderLayout.SOUTH);

        // adds listener to make xlatDestDirText identicl to
        // imageDestDirText -- when imageDestDirText is typed into.
        imageDestDirText.addKeyListener(new KeyAdapter() { // make the field

                /**
                 * When enter is typed, the next focusable component
                 * is found; the rest of the event is ignored. transmits the new text from the evt.component (the
                 * textbox) when [bs] or keys are typed.  Also handles removing selected text.  All handling is done
                 * through a StringBuffer.
                 */
                public void keyTyped(KeyEvent evt) { // not accept letters

                    JTextField t = (JTextField) evt.getComponent();
                    char ch = evt.getKeyChar();
                    int code = evt.getKeyCode();

                    if (code == KeyEvent.VK_ENTER) { // send enter key to do something else

                        // ((JComponent)t.getNextFocusableComponent()).requestFocus();
                        return;
                    }

                    if (code == KeyEvent.VK_COPY) {
                        return;
                    }

                    StringBuffer dirname = new StringBuffer(t.getText());

                    // insert into textline if a printable character.
                    if (ch !=
                            KeyEvent.VK_BACK_SPACE /*&&
                                                        * code != KeyEvent.VK_DELETE     && code != KeyEvent.VK_COPY && code
                                                        * != KeyEvent.VK_CUT        && code != KeyEvent.VK_UNDO
                                                        *  &&code != KeyEvent.VK_PASTE */) {
                        dirname.insert(t.getCaretPosition(), ch);

                        if (t.getSelectedText() != null) {
                            dirname.delete(t.getSelectionStart(), t.getSelectionEnd());
                        }
                    }

                    if (ch == KeyEvent.VK_BACK_SPACE) {

                        if (t.getSelectedText() == null) {

                            if (t.getCaretPosition() > 0) {
                                dirname.deleteCharAt(t.getCaretPosition() - 1);
                            }
                        } else {
                            dirname.delete(t.getSelectionStart(), t.getSelectionEnd());
                        }
                    }

                    xlatDestDirText.setText(dirname.toString());
                }

                /**
                 * Transmits the new text from the evt.component (the textbox) when the delete key is pressed.
                 */
                public void keyReleased(KeyEvent evt) { // not accept letters

                    JTextField t = (JTextField) evt.getComponent();
                    char code = evt.getKeyChar();

                    // DELETE key does not respond to a keyTyped event.
                    if (code == KeyEvent.VK_DELETE) { // does this ever get used??

                        // i can't tell you why i don't need to delete from a buffer .....
                        xlatDestDirText.setText(t.getText());
                    }
                }
            });

        return imagePanel;
    }

    /**
     * creates the destination panel which consists of the directory textline, the browse button, and a sub-panel to
     * provide the name of the randomized image.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildImageDestPanel() {
        JPanel destp = new JPanel(new BorderLayout());
        destp.setBorder(buildTitledBorder("Image destination directory"));

        imageDestDirText = new JTextField();
        imageDestDirText.setFont(MipavUtil.font12);
        imageDestDirText.setText(System.getProperties().getProperty("user.home"));
        destp.add(imageDestDirText, BorderLayout.CENTER);

        imageDestBrowse = new JButton("Browse");
        imageDestBrowse.setActionCommand("destination image browse");
        imageDestBrowse.setPreferredSize(MipavUtil.defaultButtonSize);
        imageDestBrowse.setFont(MipavUtil.font12B);
        imageDestBrowse.addActionListener(this);
        destp.add(imageDestBrowse, BorderLayout.EAST);

        return destp;
    }

    /**
     * creates a panel for the output log.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildLogPanel() {
        JPanel logpan = new JPanel(new BorderLayout());

        logPane = new JTextArea();

        // logPane.setRows(6);
        logPane.setEditable(false);
        logPane.setFont(MipavUtil.font12);

        JScrollPane lpane = new JScrollPane(logPane);
        lpane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
        logpan.add(lpane);

        return logpan;
    }

    /**
     * Creates the menu bar for the dialog. Builds a small menu with "New directory" and "Refresh directory" options.
     *
     * @return  DOCUMENT ME!
     */
    private JMenuBar buildMenuEntries() {
        JMenuBar anonBar = new JMenuBar();

        // builds the directory menu.
        JMenu directory = new JMenu("Directory Root");
        JMenuItem itemNew;
        JMenuItem itemRenew;

        directory = new JMenu("File");
        itemNew = new JMenuItem("New directory...");
        itemRenew = new JMenuItem("Renew list");

        directory.setFont(MipavUtil.font12B);
        directory.setMnemonic(KeyEvent.VK_F);
        itemNew.setFont(MipavUtil.font12B);
        itemNew.setActionCommand("New");
        itemNew.setAccelerator(KeyStroke.getKeyStroke('N', java.awt.Event.ALT_MASK));
        itemNew.setMnemonic(KeyEvent.VK_N);
        itemNew.addActionListener(this);
        itemRenew.setFont(MipavUtil.font12B);
        itemRenew.setActionCommand("Renew");
        itemRenew.setAccelerator(KeyStroke.getKeyStroke('R', java.awt.Event.ALT_MASK));
        itemRenew.setMnemonic(KeyEvent.VK_R);
        itemRenew.addActionListener(this);

        directory.add(itemNew);
        directory.add(itemRenew);
        anonBar.add(directory);

        // builds the clear menu
        JMenu entry = new JMenu("Dialog Entries");
        JMenuItem logClear;
        logClear = new JMenuItem("Clear Log Window");

        entry.setFont(MipavUtil.font12B);
        entry.setMnemonic(KeyEvent.VK_E);
        logClear.setFont(MipavUtil.font12B);
        logClear.setActionCommand("clear log");
        logClear.setAccelerator(KeyStroke.getKeyStroke('C', java.awt.Event.ALT_MASK));
        logClear.setMnemonic(KeyEvent.VK_C);
        logClear.addActionListener(this);

        entry.add(logClear);
        anonBar.add(entry);

        return anonBar;
    }

    /**
     * creates a name-suggestion panel.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildNameSuggestionPanel() {
        JPanel namesp = new JPanel();
        namesp.setBorder(buildTitledBorder("Anonymized names"));

        Insets ileft = new Insets(1, 2, 0, 0);
        Insets iright = new Insets(1, 15, 0, 1);
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        namesp.setLayout(gbl);

        // left side
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = ileft;
        gbc.gridwidth = GridBagConstraints.RELATIVE;

        JLabel imageNameLabel = new JLabel("Anonymous name");
        imageNameLabel.setFont(MipavUtil.font12);
        imageNameLabel.setForeground(Color.black);
        namesp.add(imageNameLabel, gbc);

        // right side
        gbc.anchor = GridBagConstraints.EAST;
        gbc.insets = iright;
        gbc.gridwidth = GridBagConstraints.REMAINDER;

        JLabel sequenceLabel = new JLabel("Sequence beginning");
        sequenceLabel.setFont(MipavUtil.font12);
        sequenceLabel.setForeground(Color.black);
        namesp.add(sequenceLabel, gbc);

        // left side
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = ileft;
        gbc.gridwidth = GridBagConstraints.RELATIVE;
        imageNameText = new JTextField(generateRandString());
        imageNameText.setFont(MipavUtil.font12);
        imageNameText.setForeground(Color.black);
        imageNameText.setColumns(15);
        namesp.add(imageNameText, gbc);
        genericImageName = imageNameText.getText();

        // right side
        gbc.anchor = GridBagConstraints.EAST;
        gbc.insets = iright;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        sequenceText = new JTextField("0");

        // sequenceText = makeNumericsOnly();
        sequenceText.setHorizontalAlignment(JTextField.RIGHT);
        sequenceText.setFont(MipavUtil.font12);
        sequenceText.setForeground(Color.black);
        sequenceText.setColumns(4);
        namesp.add(sequenceText, gbc);
        sequenceStart = 0;

        return namesp;
    }

    /**
     * Creates a new directory tree starting with <code>directory</code> as the root. Each leaf of the tree is populated
     * by <code>ViewFileTreeNode</code>s, representing directories.
     *
     * <p>It is selectable with TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION. Each item that is selected will be
     * placed into a JTextArea displaying all the selected items. And as the tree is expanded, the leaves will be
     * explored, but as yet, there is no utility to un-explore them, or refresh their view once explored.</p>
     *
     * @param   directory  a string signifying the root of the tree
     *
     * @return  a directory-tree, with the input argumenbt as the root.
     *
     * @see     ViewFileTreeNode
     * @see     TreeSelectionModel#DISCONTIGUOUS_TREE_SELECTION
     */
    private JTree buildNewSourceTree(String directory) {

        if (directory.endsWith(":\\")) {
            int index = directory.lastIndexOf('\\');
            String temp = directory.substring(0, index - 1);
            index = temp.lastIndexOf('\\');

            if (index > -1) {
                directory = directory.substring(index + 1);
            }
        }

        DefaultMutableTreeNode fs = new DefaultMutableTreeNode("Computer");

        if (fs != null) { // fs is null when the set of roots could not be determined
            fs.add(new ViewFileTreeNode(new File(directory), true));
            directoryTree = new JTree(fs);
            directoryTree.setRootVisible(false);
        } else { // we can build an empty tree, but it won't mean anything.  throw error??  FIXME
            directoryTree = new JTree();
        }

        directoryTree.getSelectionModel().setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);

        /* re-create the 'selected directory' list by reacting to
         * each selection on the tree.  on selection, the selected list is cleared, and all current selections of the
         * tree are pasted into the selected list text box.
         *
         * Note: no check has been made to remove child-nodes from a selection when the parent has been selected.
         */
        directoryTree.addTreeSelectionListener(new TreeSelectionListener() {
                public void valueChanged(TreeSelectionEvent e) {
                    DefaultMutableTreeNode node = (DefaultMutableTreeNode)
                                                      directoryTree.getLastSelectedPathComponent();

                    if (node == null) {
                        return;
                    }

                    selectedList.setText(""); // clear old text

                    TreePath[] selected = directoryTree.getSelectionPaths();

                    for (int i = 0; i < selected.length; i++) {

                        // add the selection in the list:
                        selectedList.append(((ViewFileTreeNode) selected[i].getLastPathComponent()).getAbsolutePath());
                        selectedList.append("\n");
                        // put all selected FileNodes into a list
                    }
                }
            });

        /* on expansion, the tree queries the selected node;
         * if a selected node has had its children previously added, the no nodes will be added, but the tree will
         * display the previously added children. Otherwize, the node will add nodes which will be displayed; each node
         * will be marked as adding only directories as child-nodes.
         */
        directoryTree.addTreeExpansionListener(new TreeExpansionListener() {
                public void treeCollapsed(TreeExpansionEvent tee) { } //

                public void treeExpanded(TreeExpansionEvent tee) {
                    TreePath path = tee.getPath();
                    ViewFileTreeNode node = (ViewFileTreeNode) path.getLastPathComponent();

                    if (!node.isExplored()) {
                        DefaultTreeModel model = (DefaultTreeModel) directoryTree.getModel();
                        node.exploreDirectoriesOnly(true);
                        node.explore();
                        model.nodeStructureChanged(node);
                    }
                }
            });

        return directoryTree;
    }

    /**
     * Creates the panel which consists of the OKAY button and the Cancel button. The OKAY button is set to read
     * &quot;Run&quot;, and the Cancel button is set to read &quot;Close&quot;.
     *
     * <p>a STOP button to stop the seperate thread is on the panel after the cancel button. The stop button reads
     * &quot;Stop&quot;.</p>
     *
     * <p>Places a &quot;Help&quot; button to the right of the &quot;Stop&quot; button.</p>
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildOKCancelPanel() {
        JPanel ocp = new JPanel(); // flow layout

        buildOKButton();
        OKButton.setText("Run");
        ocp.add(OKButton);

        buildCancelButton();
        cancelButton.setText("Close");
        ocp.add(cancelButton);

        buildStopButton();
        stopButton.setText("Stop");
        ocp.add(stopButton);

        ocp.add(buildHelpButton());
        helpButton.setEnabled(false); // remove when this sucker has helptext!

        return ocp;
    }

    /**
     * creates the random-button panel,which are two radio-buttons determining the naming actions for the output
     * directories.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildOptionPanel() {
        JPanel southp = new JPanel();
        southp.setBorder(this.buildTitledBorder("Other options"));
        recursiveCheckBox = new JCheckBox("Recursive anonymization");
        recursiveCheckBox.setToolTipText("Images in sub-directories will also " + "be anonymized");
        recursiveCheckBox.setSelected(false);
        recursiveCheckBox.setFont(MipavUtil.font12);
        southp.add(recursiveCheckBox);

        changeFilenameBox = new JCheckBox("Anonymize filename");
        changeFilenameBox.setToolTipText("Filenames will be anonymized");
        changeFilenameBox.setSelected(false);
        changeFilenameBox.setFont(MipavUtil.font12);
        southp.add(changeFilenameBox);


        return southp;
    }

    /**
     * creates the random-button panel,which are two radio-buttons determining the naming actions for the output
     * directories.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildRandButtonPanel() {
        JPanel randp = new JPanel();
        randp.setLayout(new BoxLayout(randp, BoxLayout.Y_AXIS));
        randp.setBorder(buildTitledBorder("Directory name anonymization"));

        grouping = new ButtonGroup();
        randTopButton = new JRadioButton("Selected directory");

        // randTopButton.setToolTipText("Applies the random name to the output "+
        // "The directory structure of the output "+
        // "gives a random name to the ");
        randTopButton.setSelected(false);
        randTopButton.setFont(MipavUtil.font12);
        grouping.add(randTopButton);
        randp.add(randTopButton);

        randChildButton = new JRadioButton("Subdirectory");
        randChildButton.setFont(MipavUtil.font12);

        // randTopButton.setToolTipText("The directory structure of the output "+
        // "gives a random name to the ");
        grouping.add(randChildButton);
        randp.add(randChildButton);

        noRandButton = new JRadioButton("No directory name change");
        noRandButton.setFont(MipavUtil.font12);

        // randTopButton.setToolTipText("The directory structure of the output "+
        // "gives a random name to the ");
        noRandButton.setSelected(true);
        grouping.add(noRandButton);
        randp.add(noRandButton);

        randp.add(Box.createHorizontalStrut(randp.getWidth() + 40));

        return randp;
    }

    /**
     * puts together the panel of recursive box and directory-randomization level radio button.
     *
     * @see     JDialogAnonymizeDirectory#buildRecursiveButtonPanel()
     * @see     JDialogAnonymizeDirectory#buildRandButtonPanel()
     *
     * @return  the loaded panel
     */
    private JPanel buildRandSelectionPanel() {
        JPanel rndp = new JPanel();
        rndp.setLayout(new BorderLayout());

        // add the recursive button and randomization rules panels
        rndp.add(buildOptionPanel(), BorderLayout.NORTH);
        rndp.add(buildRandButtonPanel(), BorderLayout.CENTER);

        return rndp;
    }

    /**
     * creates the visual display in which to list all selected directories in the directory tree. The panel is 240
     * pixels wide though that is <i>supposed</i> to be the minimum size
     *
     * @return  the panel which is to hold the list of selected items
     */
    private JPanel buildSelectedListing() {
        JPanel selp = new JPanel(new BorderLayout());
        selp.add(Box.createHorizontalStrut(240), BorderLayout.NORTH); // width of text area.  seems to start out very
                                                                      // skinny.
        selectedList = new JTextArea();
        selectedList.setEditable(false);
        selectedList.setFont(MipavUtil.font12);

        selp.add(new JScrollPane(selectedList), BorderLayout.CENTER);

        return selp;
    }

    /**
     * creates the source panel which consists of the directory line, the browse button, and a check box approving the
     * anonymize in sub-directories.
     *
     * @param   dir  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildSourcePanel(String dir) {
        JPanel srcp = new JPanel(new BorderLayout());
        srcp.setBorder(buildTitledBorder("Image source directory"));

        srcp.add(buildSourceTreeListing(dir), BorderLayout.CENTER);
        srcp.add(buildSelectedListing(), BorderLayout.EAST);

        return srcp;
    }

    /**
     * Creates the panel holding the directory tree.
     *
     * @param   directory  DOCUMENT ME!
     *
     * @return  Panel.
     */
    private JPanel buildSourceTreeListing(String directory) {
        JPanel srctreep = new JPanel(new BorderLayout());
        directoryTree = buildNewSourceTree(directory);

        JScrollPane jsp = new JScrollPane(directoryTree);
        srctreep.add(jsp, BorderLayout.CENTER);

        return srctreep;
    }

    /**
     * Builds the Stop button.
     */
    private void buildStopButton() {
        stopButton = new JButton("Stop");
        stopButton.addActionListener(this);
        stopButton.setMinimumSize(MipavUtil.defaultButtonSize);
        stopButton.setPreferredSize(MipavUtil.defaultButtonSize);
        stopButton.setFont(serif12B);
        stopButton.setEnabled(false);
    }

    /**
     * creates the destination panel which consists of the directory textline, the browse button for the translation key
     * file.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildXLATdestPanel() {
        JPanel destp = new JPanel(new BorderLayout());
        destp.setBorder(buildTitledBorder("Translation/Key file destination directory"));

        xlatDestDirText = new JTextField();
        xlatDestDirText.setFont(MipavUtil.font12);
        xlatDestDirText.setText(System.getProperties().getProperty("user.home"));
        destp.add(xlatDestDirText, BorderLayout.CENTER);

        xlatDestBrowse = new JButton("Browse");
        xlatDestBrowse.setActionCommand("xlat destination browse");
        xlatDestBrowse.setPreferredSize(MipavUtil.defaultButtonSize);
        xlatDestBrowse.setFont(MipavUtil.font12B);
        xlatDestBrowse.addActionListener(this);
        destp.add(xlatDestBrowse, BorderLayout.EAST);

        return destp;
    }

    /**
     * generates a random, up to-5 character, hexadecimal string.
     *
     * @return  a hexadeciaml string of up to 5-characters.
     */
    private String generateRandString() {
        String rhs = Integer.toHexString(rand.nextInt());

        try {
            return rhs.substring(0, 5);
        } catch (IndexOutOfBoundsException ioobe) {
            return rhs;
        }
    }

    /**
     * Makes the image destination directory. If there is a problem in creating the destination directory, the
     * "Directory" tab is brought to the front; the warning message describing the error is displayed; then the text
     * field with the destination directory is given focus, and all available text is displayed.
     *
     * @return  boolean if the selected destination has been made, returns <code>true</code>. Otherwise, returns <code>
     *          false</code>.
     */
    private boolean isImageDestinationOkay() {

        try {
            imageDestDirectory = makeDirectory(imageDestDirectory, imageDestDirText);
        } catch (IOException ioe) {
            everything.setSelectedIndex(DIR_TAB);

            if (noisyProcess) {
                MipavUtil.displayWarning(ioe.getMessage());
            }

            imageDestDirText.requestFocus();
            imageDestDirText.selectAll();

            return false;
        }

        return true;
    }

    /**
     * checks to see if the JPanelAnonymizeImage has had any selections made to it. If it hasn't, then the "Tag Options"
     * tab is brought to the front (it contains the JPanelAnonymizeImage), and the warning message <i>"No fields to
     * anonymize were selected! Select a field."</i> is displayed.
     *
     * @return  boolean if a selection in the JPanelAnonymizeImage has been made, returns <code>true</code>. Otherwise,
     *          returns <code>false</code>.
     *
     * @see     JPanelAnonymizeImage
     */
    private boolean isRemovalSelectionOkay() {

        if (checkBoxPanel.getNumberSelected() == 0) {
            //everything.setSelectedIndex(ANON_TAB);
            MipavUtil.displayWarning("No fields to anonymize were selected!  Select a field.");

            return false;
        }

        return true;
    }

    /**
     * Records all paths chosen in the source tree. If no selections have been made, the "Directory" tab is brought to
     * the front, and the warning message <i>"Select at least one directory to work on."</i> is displayed.
     *
     * @return  boolean if a selection in the source tree has been made, returns <code>true</code>. Otherwise, returns
     *          <code>false</code>.
     */
    private boolean isSelectedPathsOkay() {

        // check for nulls, report an error.
        actOnPath = directoryTree.getSelectionPaths();

        if (actOnPath == null) {
            everything.setSelectedIndex(DIR_TAB);
            MipavUtil.displayWarning("Select at least one directory to work on.");

            return false;
        }

        return true;
    }
    


    /**
     * Locates the translation/key-file destination directory. If the directory is not there already, the directory is
     * made. If there is a problem in creating the destination directory, the "Directory" tab is brought to the front;
     * the warning message describing the error is displayed; then the text field with the destination directory is
     * given focus, and all available text is displayed. If the translation/key-file is already present in the chosen
     * directory, there will be an options dialog allowing the user to Overwrite the old translation/key-file, to append
     * the the new translation/key to the end of the current file, or to cancel the entire operation (to look for a new
     * location).
     *
     * @return  boolean if the selected destination has been made, returns <code>true</code>. Otherwise, returns <code>
     *          false</code>.
     */
    private boolean isXLATdestinationOkay() {

        try {
            xlatDestDirectory = makeDirectory(xlatDestDirectory, xlatDestDirText);
        } catch (IOException ioe) {
            everything.setSelectedIndex(DIR_TAB);

            if (noisyProcess) {
                MipavUtil.displayWarning(ioe.getMessage());
            }

            xlatDestDirText.requestFocus();
            xlatDestDirText.selectAll();

            return false;
        }

        // if there is a 'patient.key.doc' file already there, ask what to do about it.
        File keyFile = new File(xlatDestDirectory, "patient.key.doc");

        try {

            if (keyFile.exists()) {
                String[] possibilities = { "Overwrite", "Append", "Cancel" };
                int result = JOptionPane.showOptionDialog(this,
                                                          "\"patient.key.doc\" already exists.  Do you want to overwrite this file?",
                                                          "File exists...", JOptionPane.YES_NO_CANCEL_OPTION,
                                                          JOptionPane.QUESTION_MESSAGE, null, possibilities,
                                                          new Integer(0));

                switch (result) {

                    case 0:
                        xlatDestinationUsage = OVERWRITE;
                        break; // user chose "OVERWRITE"

                    case 1:
                        xlatDestinationUsage = APPEND;
                        break; // user chose "APPEND"

                    case 2:
                    default:
                        return false; // user chose "CANCEL"
                }
            } else {
                xlatDestinationUsage = WRITE;
            }
        } catch (SecurityException se) {

            if (noisyProcess) {
                MipavUtil.displayError("security violation incurred while creating \"patient.key.doc\"; \n" +
                                       "is destination directory still writable?  Key translation file not written.");
            }

            Preferences.debug("security violation incurred while creating \"patient.key.doc\";\n");

            return false;
        }

        return true;
    }

    /**
     * makes the submitted directory as given by the inputted File, or the text as given in the given given textField.
     * Priority is given to the text in the textfield, so if the File's path does not agree with the path in the text
     * field, the directory will be made with the path in the text field. Should the text field not have <i>any</i>
     * path, an IOException will be thrown indicating this problem. If the directory does not exist, it will be created;
     * if there are errors in creating the directory, an IOException will be thrown, with error message describing the
     * problem, with a possible remedy as a suggestion.
     *
     * @param      selDir    DOCUMENT ME!
     * @param      txtField  DOCUMENT ME!
     *
     * @return     File a directory
     *
     * @exception  IOException  -- failure to create the directory
     */
    private File makeDirectory(File selDir, JTextField txtField) throws IOException {

        // make a 'destDirectory' file from the dir text if it isn't already made.
        if (selDir == null) { // didn't use the file selector,

            // so try to make a file out of what's been typed-in.
            if (txtField.getText().equals("")) {
                throw new IOException("No directory to make!  Select Directory.");
            }

            selDir = new File(txtField.getText());
        }

        // verify the destination directory exists, and make it if it doesn't.
        // security violation if no write-privelages?
        if (!selDir.equals(txtField.getText())) {

            // so try to make a file out of what's been typed-in.
            if (txtField.getText().equals("")) {
                throw new IOException("No directory to make!  Select Directory.");
            }

            selDir = new File(txtField.getText());

            if (!selDir.exists()) {

                try { // do we have rights to write here?  and an error if we can't?

                    if (!selDir.mkdir()) {
                        throw new IOException("Error in creating destination directory.  Write rights maybe?");
                    }
                } catch (SecurityException se) {
                    throw new IOException("security error in " + "creating destination directory:" + se.getMessage());
                }
            }
        }

        return selDir;
    }

    /**
     * creates a new keylog, writing which tags are to be removed from the image information; the table header for the
     * image read/write logging is added. the string created here is not automatically turned into the keylog string.
     * that must be done by the caller.
     *
     * @return  the new KeyLog String.
     */
    private String newKeyLog() {
        //int i;
        String kl = "";
        // kl ="#\tMIPAV will attempt to remove all of the following tags \n" +
        // "#\tfrom the images it processes.\n" +
        // "#\tTags in this list may not actually be anonymized in the \n" +
        // "#\toriginal image if they are not in the image-set.\n";

        // for (i = 0; i < FileInfoDicom.anonymizeTagIDs.length; i++) {
        // if (checkBoxPanel.getSelectedList(i)) {
        //// Dave !!                kl += "\t\t"+FileInfoDicom.anonymizeTagIDs[i]+"\t"+DicomDictionary.getName(FileInfoDicom.anonymizeTagIDs[i])+"\n";
        // }
        // }
        kl += "\n#\tsource-file : destination-file\n";

        return kl;
    }

    /**
     * creates a keyFile named "patient.key.doc" in the destination directory as specified by the user. the keyFile
     * contains a tab-delimited table of original filenames and the anonymous filename which replaced it; it flushes the
     * keyLog into the keyFile. Any further writes to the keyLog will be writing into a new keyLog. Writing to the
     * keyFile later will overwrite the keyFile already there.
     */
    private void writeKeyFile() {
        FileWriter keyFW;
        File keyFile = new File(xlatDestDirectory, "patient.key.doc");

        try {

            if (keyFile.exists()) {

                if (xlatDestinationUsage == OVERWRITE) {
                    FileDeleter fd = new FileDeleter(keyFile.getPath());
                    fd.start();
                } else if (xlatDestinationUsage == APPEND) { }
            }
        } catch (SecurityException se) {

            if (noisyProcess) {
                MipavUtil.displayError("security violation incurred while creating \"patient.key.doc\"; \n" +
                                       "is destination directory still writable?  Key translation file not written.");
            }

            Preferences.debug("security violation incurred while creating \"patient.key.doc\";\n");

            return;
        }

        try {

            if (!keyFile.createNewFile()) { // System.out.println("keyFile : "+keyFile.toString()+" not made.");
            }
        } catch (IOException io) {
            Preferences.debug("IO exception while writing Anonymizer's \"patient.key.doc\"\n");

            return;
        } catch (SecurityException se) {

            if (noisyProcess) {
                MipavUtil.displayError("security violation incurred while creating \"patient.key.doc\"; \n" +
                                       "is destination directory still writable?  Key translation file not written.");
            }

            Preferences.debug("security violation incurred while creating \"patient.key.doc\";\n");

            return;
        }

        try {

            if (xlatDestinationUsage == OVERWRITE) {
                keyFW = new FileWriter(keyFile);
                keyFW.write(keyLog, 0, keyLog.length());
            } else if (xlatDestinationUsage == APPEND) {
                keyFW = new FileWriter(keyFile.getAbsolutePath(), true);
                keyFW.write(keyLog);
            } else { // WRITE
                keyFW = new FileWriter(keyFile);
                keyFW.write(keyLog, 0, keyLog.length());
            }

            keyFW.close();
        } catch (IOException ioe) {

            if (noisyProcess) {
                MipavUtil.displayError("error writing the logging to \"patient.key.doc\"");
            } // figure out where to store somewhere else?
        }

        keyLog = newKeyLog();
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * This thread anonymization processes all the Dicom files in the submitted source directories and places them into
     * the submitted destination directory. Note that these selected directories are processed serially.
     *
     * <p>When one directory is chosen as the source, all anonymized Dicom files in source are placed directly in the
     * destination. Any images in any subdirectories (when chkChildren is selected) are sent into the children of the
     * sub-directories.</p>
     *
     * <p>More than one submitted source directory will be placed into discrete child directories of the submitted
     * destination directory. As as example, /images/a, /images/b are selected, and /dest is the selected destination
     * directory. They would get put into /dest/a, and /dest/b.</p>
     *
     * <p>Naming can change the way the destination is made. Selecting a top-level randomisation, will randomate the
     * output directory ofthe selected image directory. This isalong the lines in the above example as /images/a
     * becoming /dest/abcde; and /images/b becoming /dest/12345. Selecting only one input directory will notmakea
     * difference for top-level randomising. Child-directory randomising will give a random name when processing child
     * direcories. this amounts to selecting /images (and recursive processing) and the destination directory /dest,and
     * getting /images/a becoming /dest/abcde, and /images/b becoming /dest/12345.</p>
     */
    public class AnonymizeDicomDirectories extends Thread {

        /** DOCUMENT ME! */
        private PlugInDialogAnonymizeGE_Genesis anonDialog; // for notification of completion

        /** DOCUMENT ME! */
        private String anonymousName; // used for the "image" directory and is the beginning

        /** DOCUMENT ME! */
        private boolean checkChildren = false;

        /** DOCUMENT ME! */
        private File[] destDirs;

        /** DOCUMENT ME! */
        private File destStub;

        /** DOCUMENT ME! */
        private boolean doRename = false;

        /** portion of the file name for all anonymized images. */
        private String genericName; // used as part of the image name; used in all processed image-sets.

        /** DOCUMENT ME! */
        private FileIO io;

        /** DOCUMENT ME! */
        private String keyLog = "";

        /** DOCUMENT ME! */
        private ModelImage mi;

        /** DOCUMENT ME! */
        private Vector<JTextArea> notifierList;

        /** DOCUMENT ME! */
        private String outputLog = "";

        /** DOCUMENT ME! */
        private int randomizeTopLevelDir = NO_RANDOM_NAMES;

        /** DOCUMENT ME! */
        private int sequence = 0; // used as part of the image name; increments for each image set processed -- that
                                  // is, the next image number

        /** DOCUMENT ME! */
        private int sequenceStart = 0; // used as part of the image name; represents beginning of sequence for all
                                       // processed image sets.

        /** DOCUMENT ME! */
        private File[] srcDirs;

        /** DOCUMENT ME! */
        private boolean stopThreadRequest = false; // describes the current running status of this Thread.

        /** DOCUMENT ME! */
        private FileWriteOptions writeOpts;

        /**
         * sets up the anonymizer thread.
         *
         * @param  src        the list of directories to anonymize. this may be as large or as small as needed
         * @param  dest       the output directory. when the source-list lists but one file, the destination is the
         *                    directory provided. If the list is more than one, then the destination is the root of the
         *                    output directorues. How the output directories are named depends on the boolean, toprand.
         * @param  aname      the name to use for all images processed.
         * @param  startseq   number to start the sequence for all names in the sequence of anonymousnames.
         * @param  toprand    determines which directory to anonymize, if any. When processing a list of more than one
         *                    source, the top-level directory can be randomized (as if the selected list were the
         *                    image-directories themselves), or the first sub-directory level of directories (as if the
         *                    selected directories had held lists of patient directories).
         *
         *                    <ul>
         *                      <li><code>NO_RANDOM_NAMES</code> indicates no directory names are to be changed.</li>
         *                      <li><code>RANDOM_DIRECTORY_NAME</code> indicates the top-level is to be randomised,</li>
         *                      <li><code>RANDOM_SUBDIRECTORY_NAME</code> indicates the first sub-directory level to be
         *                        randomised.</li>
         *                    </ul>
         *
         *                    <p>There is (true has??) no effect when only one level randomised.</p>
         * @param  do_rename  DOCUMENT ME!
         */
        AnonymizeDicomDirectories(File[] src, File dest, String aname, int startseq, int toprand, boolean do_rename) {
            super("Anonymize Dicom directories");

            // System.err.println("DO RENAME IS: " + do_rename);
            randomizeTopLevelDir = toprand;
            doRename = do_rename;
            srcDirs = src;
            destStub = dest;

            if (src.length == 1) {
                destDirs = new File[1];
                // destDirs[0] = dest;
            } else {
                destDirs = new File[src.length];
            }

            setGenericName(aname, startseq);
            io = new FileIO();
            io.setQuiet(true);
            writeOpts = new FileWriteOptions(doRename);

            setPriority(Thread.MIN_PRIORITY);
        }

        /**
         * a list of JTextAreas to be notified when a change in the process/output log is made. this permits
         * almost-real-time updates to output devices this method adds the selected list of JTextAreas to receieve the
         * output logs.
         *
         * @param  jta  DOCUMENT ME!
         */
        public void addProcessLoggingNotifier(JTextArea jta) {

            try {

                if (notifierList == null) {
                    notifierList = new Vector<JTextArea>();
                }

                notifierList.add(jta);
            } catch (OutOfMemoryError oome) {
                Preferences.debug("out of memory creating anonymizer notifier vector\n");
            }
        }

        /**
         * provides the string key of translations between source filename and destination filename. The string is
         * formatted with newlines and tabs seperating the filenames.
         *
         * @return  String the translation Strin as formatted by the anonymizer
         */
        public String getKeyLog() {
            return keyLog;
        }

        /**
         * provides the string key of translations between source filename and destination filename. The string is
         * formatted with newlines and tabs seperating the filenames.
         *
         * @return  String the translation Strin as formatted by the anonymizer
         */
        public String getProcessLog() {
            return outputLog;
        }

        /**
         * Specified by Thread. processes each directory selected directory, pausing after processing each selected
         * directory to allow other threads run. Notifies the 'anonDialog' of completion.
         */
        public void run() {
            stopThreadRequest = false; // this thread is now running.
            writeToLog("------------------------------------------", true);
            writeToLog("Running new anonymization on " + srcDirs.length + " director" +
                       ((srcDirs.length == 1) ? "y" : "ies") + ":", true);

            for (int i = 0; i < srcDirs.length; i++) {

                try {
                    sleep(200);
                } catch (InterruptedException ie) {
                    Preferences.debug("Thread interrupted!\n", Preferences.DEBUG_MINOR);
                }

                /* anonymousName is used in creating the
                 * destination directory when we are randomizing on the top-level-directory. However, although it is
                 * unlikely to be true, we cannot assume that there are no DICOM images in the top directory.  We must
                 * generate an anonymous name to handle when there are DICOM images to anonymize there.
                 */
                anonymousName = generateAnonymousName();

                // assumed none of the outpout dirs are children of the input dirs
                // random destination directories?
                switch (randomizeTopLevelDir) {

                    case RANDOM_DIRECTORY_NAME:
                        destDirs[i] = new File(destStub, anonymousName);
                        break;

                    case RANDOM_SUBDIRECTORY_NAME:
                    case NO_RANDOM_NAMES:
                    default:
                        destDirs[i] = new File(destStub, srcDirs[i].getName());
                        break;
                }

                destDirs[i].mkdir(); // make directory, using name selected
                processMainDirectory(srcDirs[i], destDirs[i]);
            }

            anonDialog.anonymizationComplete(this);
        }

        /**
         * tells the anonymizer to check and anonymize the child directories of the selected source directories. Default
         * is false;
         *
         * @param  chk  whether or not to check the child directories.
         */
        public void setCheckChildren(boolean chk) {
            checkChildren = chk;
        }

        /**
         * no real need for complete listener capabilities, so this is to take its place.
         *
         * @param  jsa  the anonimizer dialog to notify of the anonymization being complete
         */
        public void setFinishNotification(PlugInDialogAnonymizeGE_Genesis jsa) {
            anonDialog = jsa;
        }

        /**
         * stopping a thread prematurely is a co-operative process where the Thread checks for a stop-flag periodically,
         * and stops itself by coming to the end of its <code>run()</code> method early. This co-operative method of
         * premature Thread stop permits the Thread to close any open system resources it needed, and will use the
         * natural death procedures.
         *
         * <p>In this case, in order to accept death, the Thread must close its open files; it will then back out of its
         * recursion.</p>
         */
        public void stopProcess() {
            stopThreadRequest = true;
            writeToLog("\n--Stop request made--", true);
        }

        /**
         * actually does the grunt-work of reading the designaated image-file, anonymizing it, and writing the file to
         * the deisgnated destination directory. Note that the anonymous-name is class-public. <i>don't overwrite
         * it!</i> This method contains a list to all the random numbers used in making the image destination due to
         * problems in making File.exists() work correctly without a SecurityManager. (haven't tried WITH a
         * SecurityManager).
         *
         * @param   imageFile             a File that <i>could</i> be a DICOM image. Non-DICOM images should fail and
         *                                simply return out of this method without another thought. DIMC images will be
         *                                processed and saved into the destination directory.
         * @param   destinationDirectory  a File which is the directory where a processed DICOM image file will be
         *                                saved.
         * @param   randomSuffixList      array of ints, which are filled from 0 to max lengh of an unsorted numbers.
         *                                each of which becomes the random name for the file.
         * @param   numberOfSuffices      current number of used slots in randomSuffixList. not to be larger than
         *                                randomSuffixList.length returns boolean if the given image file was properly
         *                                written to the destination directory, this method returns <code>true</code>.
         *                                <code>false</code> gets returned if the file either did not get written
         *                                correctly (which would generate an exception), or if the file was non-DICOM,
         *                                and was therefor ignored.
         *
         * @return  DOCUMENT ME!
         *
         * @throws  IllegalArgumentException  when destinationDirectory.isDirecrtory() returns <code>false</code>
         */
        private boolean anonymizeImage(File imageFile, File destinationDirectory, int[] randomSuffixList,
                                       int numberOfSuffices) {
            String anonName;
            int randomNumber;

            if (!destinationDirectory.isDirectory()) {
                throw new IllegalArgumentException(destinationDirectory.toString() + " is not a directory.");
            }

            try {
                //String suffix = FileUtility.getExtension(imageFile.getName());
                //if (!suffix.equalsIgnoreCase(".sig")) {
                    if (FileUtility.isGESigna5X(imageFile.getName(), imageFile.getParent() + File.separator, true) != FileUtility.GE_GENESIS) {
                        writeToLog(imageFile.getAbsolutePath(), false);
                        writeToLog(" -- Not a GE_Genesis file.  File skipped.", true);
                        return false;
                    }
                //}
                
                io.setFileDir(imageFile.getParent() + File.separator);
                mi = io.readGEGenesis5X(imageFile.getName(), imageFile.getParent() + File.separator);
                writeToLog(imageFile.getAbsolutePath(), false);

                if (mi != null) {
                    mi.anonymize(null, doRename);

                    try {

                        do {

                            // create a unique filename using the anonymousName (a generic name
                            // concatonated with unique integer identifier)
                            randomNumber = (int) (Math.random() * 1000000);
                            // come up with another name, can cause permenant lock if all available choices are made
                            // (ie., more than 1000000 imags are in the directory)
                        } while (exists(randomNumber, randomSuffixList, numberOfSuffices));

                        randomSuffixList[numberOfSuffices] = randomNumber;
                    } catch (ArrayIndexOutOfBoundsException aioobe) {
                        writeToLog("\n --- too many image files!  Cannot create any more random names.  Quitting this directory");

                        return false;
                    }

                    anonName = anonymousName + "_" + Integer.toString(randomNumber) + ".dcm";

                    if (doRename) {
                        writeOpts.setFileName(anonName);
                        mi.setImageName(anonymousName);
                    } else {

                        //System.err.println("FILE NAME IS: " + mi.getImageFileName());
                        writeOpts.setFileName(mi.getImageFileName());
                    }

                    writeOpts.setFileDirectory(destinationDirectory.getPath() + File.separator);
                    writeOpts.setOptionsSet(true);
                    writeOpts.setRecalculateInstanceNumber(false);
                    writeOpts.doPutInQuicklist(false);
                    writeOpts.setSaveInSubdirectory(false); // should be false by default.  we want to preserve current
                                                            // writeOpt structure

                    io.writeImage(mi, writeOpts);
                    // log change.

                    if (doRename) {
                        writeToLog(writeOpts.getFileDirectory() + anonName, true);
                        writeToKey(imageFile.getAbsolutePath(), writeOpts.getFileDirectory() + anonName);
                    } else {
                        writeToLog(writeOpts.getFileDirectory() + writeOpts.getFileName(), true);
                        writeToKey(imageFile.getAbsolutePath(), writeOpts.getFileDirectory() + writeOpts.getFileName());
                    }


                    // clean up for garbage-collector
                    mi.disposeLocal();
                    mi = null;

                    return true;
                } else { // not a GE_Genesis file.  leave it.
                    writeToLog(" -- Not a GE_Genesis file.  File not Touched. Model Image = null.", true);

                    return false;
                }
            } catch (OutOfMemoryError oome) {
                Preferences.debug("System out of memory.  level too deep.  coming up to a higher level.\n");
                writeToLog("System out of memory.  Will attempt to process other directories.", true);
                mi = null;
                throw oome;
            } catch (NegativeArraySizeException nase) {
                Preferences.debug("anonymizeImage: (Negative Array Size Error " + "while reading: >" + imageFile +
                                  "< .  try a different file.\n");
                writeToLog(imageFile.getAbsolutePath() + "  -- Not a GE_Genesis file.  File not Touched.", true);
                mi = null;

                return false;
            } catch (IOException ioe) {
                writeToLog(imageFile.getAbsolutePath(), false);
                writeToLog(" -- Problem encountered reading file.", true);
                
                ioe.printStackTrace();
                mi = null;
                
                return false;
            }
        }

        /**
         * Checks for the existance of the number <code>rnum</code> in the unsorted array rlist. The elements of <code>
         * rlist</code> exist from <code>rlist[0]</code> to <code>rlist[rnumptr-1]</code>. Of course, if <code>
         * rnumptr</code> is greater than to <code>rlist.length</code>, an <code>ArrayIndexOutOfBoundsException</code>
         * will be thrown. Check for it. This of course means that <code>rnum</code> is not in the full array. This
         * method does not check the array against the length of the array, but rather the number of assigned members,
         * because only a partially-full list would return unpredicatable results.
         *
         * @param   rnum     number we are interested in finding.
         * @param   rlist    unsorted array of integers at the bottom elements.
         * @param   rnumptr  index into the array to stop searching; also, indicates the number of elements with an
         *                   assigned value.
         *
         * @return  <code>true</code> when <code>rnum</code> is found between <code>rlist[0]</code> and <code>
         *          rlist[rlistptr-1]</code>. <code>false</code> when <code>rnum</code> is not in the array.
         */
        private boolean exists(int rnum, int[] rlist, int rnumptr) {

            for (int i = 0; i < rnumptr; i++) {

                if (rlist[i] == rnum) {
                    return true;
                }
            }

            // else
            return false;
        }

        /**
         * creates a string to use as a name out of the generic name, and the sequence. When processed, the anonymous
         * name will be genericname_[seq]. the following name will be genericname_[seq+1].
         *
         * @return  the anonymous name.
         */
        private String generateAnonymousName() {
            anonymousName = genericName + "_" + Integer.toString(sequence++);

            return (anonymousName);
        }

        /* /** generates a random, upto-5 character hexadecimal string.
         *   @return a hexadeciaml string of upto 5-characters.
         */

        /*private String generateRandString() {
         * String rhs = Integer.toHexString(rand.nextInt()); try {return rhs.substring(0,5);}
         * catch(IndexOutOfBoundsException ioobe) { return rhs; } } */

        /**
         * convienience method to add the filename to the log-string.
         *
         * <p>Appends a newline to the file.</p>
         *
         * @param  f  -- file to note the conversion of the file to log.
         */
        private void logWriting(File f) {
            writeToLog(f.toString(), true);
        }

        /**
         * called when an update is made to the output log, this method updates all the process logging JTextAreas.
         *
         * @param  logtext  DOCUMENT ME!
         */
        private void notifyProcessLogs(String logtext) {

            if (notifierList == null) {
                return;
            }

            for (int i = 0; i < notifierList.size(); i++) {

                try {
                    ((JTextArea) notifierList.elementAt(i)).append(logtext);
                } catch (NullPointerException npe) {
                    System.out.println("null");
                }
            }
        }

        /**
         * opens top-level image directory. anonymizes the images in the top directory, when the class has been set to
         * dig in recursively, anonymizes the top directories and calls openSubDirectory(...) to anonymize the files
         * below.
         *
         * @param  sourceDirectory       DOCUMENT ME!
         * @param  destinationDirectory  DOCUMENT ME!
         */
        private void processMainDirectory(File sourceDirectory, File destinationDirectory) {
            Vector<File> childDirsVector = new Vector<File>();
            File childDest;
            File[] filesHere = sourceDirectory.listFiles();
            int[] randomSuffixList = new int[filesHere.length];
            int numberSuffices = 0;
            int i;

            writeToLog(anonymousName);

            for (i = 0; i < filesHere.length; i++) {

                if (filesHere[i].isDirectory()) {
                    childDirsVector.add(filesHere[i]);
                } else {

                    try {

                        if (anonymizeImage(filesHere[i], destinationDirectory, randomSuffixList, numberSuffices)) {
                            numberSuffices++;
                        }
                    } catch (IllegalArgumentException ale) {
                        Preferences.debug("bug found.\n");
                        writeToLog("Bug Found.", true);
                        ale.printStackTrace();
                    } catch (OutOfMemoryError oome) {
                        Preferences.debug("Too deep a level; returning to process at a different level.\n");
                        writeToLog("System out of memory.  Will attempt to process other directories.", true);

                        return;
                    }

                    // comply with request has been made to stop thread:
                    if (stopThreadRequest) {
                        writeToLog("\n\nProcess stopped.  Anonymize process has been cancelled.", true);

                        return;
                    }
                }
            }

            if (checkChildren) {

                if (!childDirsVector.isEmpty()) {

                    /* for each of the child directories,
                     * so long as a stopThreadRequest has not come in, make a new random name, and then create a
                     * subdirectory in the destination direcoty with that random name; run clean-up, then append what
                     * was done (and to what) to the logs.
                     */
                    for (i = 0; i < childDirsVector.size(); i++) {

                        // comply with request has been made to stop thread:
                        if (stopThreadRequest) {
                            writeToLog("\n\nProcess stopped.  Anonymize process has been cancelled.", true);

                            return;
                        }

                        // random string generate
                        switch (randomizeTopLevelDir) {

                            case RANDOM_SUBDIRECTORY_NAME:

                                // give the destination driectory a random Name and hold onto it.
                                anonymousName = generateAnonymousName();
                                childDest = new File(destinationDirectory, anonymousName);
                                break;

                            case RANDOM_DIRECTORY_NAME:
                            case NO_RANDOM_NAMES:
                            default:

                                // hold onto the random name for all subdir filenames, just make a new
                                // subdir with the same dirname as the source's subdir.
                                childDest = new File(destinationDirectory, ((File) childDirsVector.get(i)).getName());
                                break;
                        }

                        try {
                            childDest.mkdir();
                        } catch (SecurityException se) {

                            if (noisyProcess) {
                                MipavUtil.displayError("Cannot anonymize \"" +
                                                       ((File) childDirsVector.get(i)).toString() + "\"");
                            }
                        }

                        System.gc();
                        System.runFinalization();
                        logWriting(childDest);
                        writeToKey(((File) childDirsVector.get(i)).getAbsolutePath(), childDest.getAbsolutePath());
                        processSubDirectory(childDest, (File) childDirsVector.get(i));
                    }
                }
            }

            randomSuffixList = null;
            filesHere = null;
            childDirsVector = null;
            childDest = null; // minor (!!) memory problems
        }

        /**
         * traverses the directory, anonymizing as it goes. the child directories are named after the source
         * child-directory, and attempted to traverse (by calling processSubDirectory(File dest, File src)).
         *
         * @param  destParentDirectory    parent directory is the parent directory tree for the destination;
         * @param  sourceParentDirectory  source name is the name of the source directory name
         *
         * @see    JDialogAnonymizeDirectory#processSubDirectory(File, File)
         */
        private void processSubDirectory(File destParentDirectory, File sourceParentDirectory) {
            File childDest;
            Vector<File> childDirsVector = new Vector<File>();
            File[] filesHere = sourceParentDirectory.listFiles();
            int[] randomSuffixList = new int[filesHere.length];
            int numberSuffices = 0;
            int i;

            // writeToLog(anonymousName);
            // writeToKey(anonymousName);
            for (i = 0; i < filesHere.length; i++) {

                if (filesHere[i].isDirectory()) {
                    childDirsVector.add(filesHere[i]);
                } else { // non-directory file.

                    try {

                        if (anonymizeImage(filesHere[i], destParentDirectory, randomSuffixList, numberSuffices)) {
                            numberSuffices++;
                        }
                    } catch (IllegalArgumentException ale) {
                        Preferences.debug("****  JDialogAnonymizeDirectory  ****  bug found.****\n");
                        ale.printStackTrace();
                    } catch (OutOfMemoryError oome) {
                        Preferences.debug("Too deep a level; returning to process at a different level.\n");
                        randomSuffixList = null;
                        childDirsVector = null;
                        childDest = null; // minor (!!) memory problems

                        return;
                    }

                    // comply with request has been made to stop thread:
                    if (stopThreadRequest) {
                        writeToLog("\n\nProcess stopped.  Anonymize process has been cancelled.", true);

                        return;
                    }
                }
            }

            if (!childDirsVector.isEmpty()) {

                /* for each of the child directories,
                 * so long as a stopThreadRequest has not come in, make a new random name, and then create a
                 * subdirectory in the destination direcoty with that random name; run clean-up, then append what was
                 * done (and to what) to the logs.
                 */
                for (i = 0; i < childDirsVector.size(); i++) {

                    // comply with request has been made to stop thread:
                    if (stopThreadRequest) {
                        writeToLog("\n\nProcess stopped.  Anonymize process has been cancelled.", true);

                        return;
                    }

                    childDest = new File(destParentDirectory, ((File) childDirsVector.get(i)).getName());

                    try {
                        childDest.mkdir();
                    } catch (SecurityException se) {
                        writeToLog("Cannot anonymize \"" + ((File) childDirsVector.get(i)).toString() + "\"", true);

                        if (noisyProcess) {
                            MipavUtil.displayError("Cannot anonymize \"" + ((File) childDirsVector.get(i)).toString() +
                                                   "\"");
                        }

                        continue;
                    }

                    logWriting(childDest);
                    writeToLog(anonymousName);
                    writeToKey(((File) childDirsVector.get(i)).getAbsolutePath(), childDest.getAbsolutePath());
                    processSubDirectory(childDest, (File) childDirsVector.get(i));
                }
            }

            // perform some clean-up so that
            // the garbage-collector can take these away:
            randomSuffixList = null;
            childDirsVector = null;
            childDest = null; // minor (!!) memory problems
        }

        /**
         * sets the genericname, and the start sequence to use when generating the anonymousname. When processed, the
         * anonymous name will be genericname_[seq]. the following name will be genericname_[seq+1].
         *
         * @param  aname     the name to use for all images processed.
         * @param  seqStart  number to start the sequence for all names in the sequence of anonymousnames.
         *
         * @see    JDialogAnonymizeDirectory#generateAnonymousName()
         */
        private void setGenericName(String aname, int seqStart) {
            genericName = (aname != "") ? aname : generateRandString();
            sequenceStart = seqStart;
            sequence = sequenceStart;
        }

        /**
         * appends the original string, then two tabs then the newKey, followed by a newline to the keylog. old keylog
         * is not overwritten.
         *
         * @param  original  DOCUMENT ME!
         * @param  newKey    DOCUMENT ME!
         */
        private void writeToKey(String original, String newKey) {
            keyLog += original + " : " + newKey + "\n";
        }

        /**
         * appends the name string, in between parens trailed by a colon. Eg., <i>(anon_name:)"</i>. To be used before
         * filtering the tags out of images (as those actions are logged in sequence.)
         *
         * @param  anon_name  DOCUMENT ME!
         */
        private void writeToLog(String anon_name) {
            notifyProcessLogs("(" + anon_name + ":)" + "\n");
            outputLog += "(" + anon_name + ":)" + "\n";
        }

        /**
         * Method to append the string to the log, adding a newline to the end, if requested, and appending a colon when
         * not adding a newline. The log is not replaced by the incoming string.
         *
         * @param  logappendage    string to write to the log
         * @param  completeString  <code>true</code> writes a newline to the log. <code>false</code> makes this an
         *                         unfinished string.
         */
        private void writeToLog(String logappendage, boolean completeString) {
            notifyProcessLogs((completeString) ? (logappendage + "\n") : (logappendage + " : "));
            outputLog += (completeString) ? (logappendage + "\n") : (logappendage + " : ");
        }
    }

}
