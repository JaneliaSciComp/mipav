package gov.nih.mipav.view.srb;


import edu.sdsc.grid.gui.*;
import edu.sdsc.grid.io.*;

import edu.sdsc.grid.io.srb.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.net.*;

import javax.swing.*;
import javax.swing.tree.*;

import gov.nih.mipav.model.srb.SRBUtility;

/**
 * A utility class which creates a JTree using the Jargon GUI package. This "Browsable DataGrid Tree" uses the
 * JargonTreeModel, JargonTreeCellRenderer, JargonTreeCellEditor.
 *
 * <P>This class can be used as is, to create a generic grid enabled file browser. Also this class is meant to provide
 * example code for those intending to create their own customized grid enabled file browser.</P>
 *
 * @author  Lucas Ammon Gilbert
 * @see     JargonTreeModel
 * @see     JargonTreeCellRenderer
 * @see     JargonTreeCellEditor
 * @since   JARGON1.5
 */
public class JSRBTree extends JTree implements ActionListener {
    //----------------------------------------------------------------------
    // Constants
    //----------------------------------------------------------------------

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6486109552343342133L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Only display files matching these conditions. */
    private MetaDataCondition[] conditions;

    /** Stores a selected abstract filepath. Presumably to be pasted somewhere else on the tree. */
    private GeneralFile copyBuffer = null;


    //----------------------------------------------------------------------
    // Fields
    //----------------------------------------------------------------------
    /**
     * The popupmenu.
     *
     * @see  <code>useDefaultPopupMenu( boolean )</code>
     */
    private JPopupMenu popup = null;

    /**
     * Watches for popup mouse click.
     *
     * @see  <code>useDefaultPopupMenu( boolean )</code>
     */
    private MouseListener popupListener;


    /** The roots of the JTree, can have more then one root. */
    private GeneralFile[] roots;

    /** Files will display these metadata values inline in the tree. */
    private MetaDataSelect[] selects;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    // ----------------------------------------------------------------------
    // Constructors & Destructors
    // ----------------------------------------------------------------------
    /**
     * Creates a new JTree using the JargonTreeModel, JargonTreeCellRenderer and JargonTreeCellEditor.
     *
     * @param   root  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public JSRBTree(GeneralFile root) throws IOException {
        this(root, null, null);
    }

    /**
     * Creates a new JTree using the JargonTreeModel, JargonTreeCellRenderer and JargonTreeCellEditor. Each GeneralFile
     * is used as the root of a subtree.
     *
     * @param   roots  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public JSRBTree(GeneralFile[] roots) throws IOException {
        this(roots, null, null);
    }

    /**
     * Creates a new JTree using the JargonTreeModel, JargonTreeCellRenderer and JargonTreeCellEditor. Metadata values,
     * as selected by the <code>selects</code> will be displayed when available.
     *
     * @param   root     DOCUMENT ME!
     * @param   selects  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public JSRBTree(GeneralFile root, MetaDataSelect[] selects) throws IOException {
        this(root, null, selects);
    }

    /**
     * Creates a new JTree using the JargonTreeModel, JargonTreeCellRenderer and JargonTreeCellEditor. Each GeneralFile
     * is used as the root of a subtree. For example, both the root of a local filesystem and the SRB root could be
     * displayed in a single JTree.
     *
     * <P>Metadata values, as selected by the <code>selects</code> will be displayed when available.</P>
     *
     * @param   roots    DOCUMENT ME!
     * @param   selects  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public JSRBTree(GeneralFile[] roots, MetaDataSelect[] selects) throws IOException {
        this(roots, null, selects);
    }

    /**
     * Creates a new JTree using the JargonTreeModel, JargonTreeCellRenderer and JargonTreeCellEditor. Metadata values,
     * as selected by the <code>selects</code> will be displayed when available. Only those files which satisfy the
     * query <code>conditions</code> will be displayed.
     *
     * @param   root        DOCUMENT ME!
     * @param   conditions  DOCUMENT ME!
     * @param   selects     DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public JSRBTree(GeneralFile root, MetaDataCondition[] conditions, MetaDataSelect[] selects) throws IOException {
        GeneralFile[] roots = { root };
        createJargonTree(roots, conditions, selects);
    }

    /**
     * Creates a new JTree using the JargonTreeModel, JargonTreeCellRenderer and JargonTreeCellEditor. Each GeneralFile
     * is used as the root of a subtree. For example, both the root of a local filesystem and the SRB root could be
     * displayed in a single JTree.
     *
     * <P>Metadata values, as selected by the <code>selects</code> will be displayed when available. Only those files
     * which satisfy the query <code>conditions</code> will be displayed.</P>
     *
     * @param   roots       DOCUMENT ME!
     * @param   conditions  DOCUMENT ME!
     * @param   selects     DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public JSRBTree(GeneralFile[] roots, MetaDataCondition[] conditions, MetaDataSelect[] selects) throws IOException {
        createJargonTree(roots, conditions, selects);
    }


    //----------------------------------------------------------------------
    // Testing Methods
    //----------------------------------------------------------------------
    /**
     * A test constructor.
     *
     * @param   args     DOCUMENT ME!
     * @param   selects  DOCUMENT ME!
     *
     * @throws  URISyntaxException  DOCUMENT ME!
     * @throws  IOException         DOCUMENT ME!
     */
    private JSRBTree(String[] args, MetaDataSelect[] selects) throws URISyntaxException, IOException {

        /**
         * As an example using this condition will only show in the tree, those files which are smaller than 2000 bytes
         */
        MetaDataCondition[] conditions = {
            MetaDataSet.newCondition(SRBMetaDataSet.SIZE, MetaDataCondition.LESS_THAN, 2000)
        };

        /**
         * Note: SRB serverside, if some of the selects don't exist, ie. the database tables are null, only those
         * results without null tables will be returned. eg. If SIZE and GUID are selected, but not all the files have
         * GUIDs, then only the files with GUIDs will be listed, even though based on the conditions one would expect
         * more files and their SIZE metadata. DEFINABLE_METADATA_FOR_FILES would actually behave the same way, but
         * certain measure were taken to compensate somewhat.
         */
        if (selects == null) {
            String[] selectFieldNames = {
                SRBMetaDataSet.FILE_COMMENTS, SRBMetaDataSet.SIZE, SRBMetaDataSet.ACCESS_CONSTRAINT,
                SRBMetaDataSet.USER_NAME, SRBMetaDataSet.DEFINABLE_METADATA_FOR_FILES,
            };
            selects = MetaDataSet.newSelection(selectFieldNames);
        }

        // create the files from URI values
        // might throw URISyntaxException
        if (args.length < 1) {
            SRBFileSystem fs = new SRBFileSystem();
            GeneralFile[] roots = {
                FileFactory.newFile(new URI("file:///")), FileFactory.newFile(fs, "/"),
                FileFactory.newFile(fs, fs.getHomeDirectory()),
            };
            //          createJargonTree( roots, conditions, selects );
            createJargonTree(roots, null, selects);
        } else {
            GeneralFile[] roots = new GeneralFile[args.length];

            for (int i = 0; i < args.length; i++) {
                roots[i] = FileFactory.newFile(new URI(args[i]));
            }

            createJargonTree(roots, null, selects);
        }

        useDefaultPopupMenu(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Stand alone testing.
     *
     * @param  args  DOCUMENT ME!
     */
    public static void main(String[] args) {

        try {
            JFrame frame = new JFrame("JSRBTree");
            JSRBTree tree = new JSRBTree(args, null);

            JScrollPane pane = new JScrollPane(tree);
            pane.setPreferredSize(new Dimension(800, 600));

            frame.addWindowListener(new WindowAdapter() {
                    public void windowClosing(WindowEvent we) {
                        System.exit(0);
                    }
                });
            frame.getContentPane().add(pane, BorderLayout.NORTH);
            frame.pack();
            frame.setVisible(true);
            frame.validate();

        } catch (Throwable e) {
            e.printStackTrace();
            System.out.println(((SRBException) e).getStandardMessage());
        }
    }

    public void delete(){
        Toolkit.getDefaultToolkit().beep();

        int reply = JOptionPane.showConfirmDialog(this, "Do you really want to delete selected items?", "Delete selected items",
                                                  JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        if (reply == JOptionPane.YES_OPTION) {
            try {

                // The node currently selected on this JSRBTree
                TreePath[] selectedPaths = this.getSelectionPaths();
                if (selectedPaths != null) {
                    for (int i = 0; i < selectedPaths.length; i++) {
                        GeneralFile selectedFile = (GeneralFile) selectedPaths[i]
                                .getLastPathComponent();
                        SRBUtility.recursivelyDeleteDir(selectedFile);
                        // that file is gone so refresh to the parent
                    }
                    refresh(selectedPaths[0].getParentPath());
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        }
    }
    /**
     * Defines the default ActionEvents.
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {
        
        String actionCommand = "Delete";

        try {

            // The node currently selected on this JSRBTree
            GeneralFile target = (GeneralFile) getLastSelectedPathComponent();

            if (actionCommand.equals("Refresh")) {
                refresh(getSelectionPath());
            } else if (actionCommand.equals("Query")) {
                /*                if (target == null) {
                                    //no file selected try using the first filesystem
                                    JFrame queryFrame = new JFrame("Query");
                                    queryFrame.getContentPane().add(new QueryPanel(
                                        roots[0].getFileSystem()));
                                    queryFrame.pack();
                                    queryFrame.setVisible(true);
                                    queryFrame.validate();
                                }
                                else {
                                    JFrame queryFrame = new JFrame("Query");
                                    queryFrame.getContentPane().add(new QueryPanel(target));
                                    queryFrame.pack();
                                    queryFrame.setVisible(true);
                                    queryFrame.validate();
                                }*/
            } else if (actionCommand.equals("Copy")) {

                if (target != null) {
                    copyBuffer = target;
                }
            } else if (actionCommand.equals("Paste")) {

                if ((copyBuffer == null) || (target == null)) {
                    return;
                }

                // Create a TransferStatusPanel to tell the user about the copy
                TransferStatusPanel transfer = null;

                if (target.isDirectory() && copyBuffer.isFile()) {
                    transfer = new TransferStatusPanel(copyBuffer, FileFactory.newFile(target, copyBuffer.getName()));
                    transfer.setOverwrite(TransferStatusPanel.OVERWRITE);

                    JFrame frame = new JFrame();
                    frame.getContentPane().add(transfer);
                    frame.pack();
                    frame.setVisible(true);
                    transfer.start();
                } else {
                    transfer = new TransferStatusPanel(copyBuffer, target);
                    transfer.setOverwrite(TransferStatusPanel.OVERWRITE);

                    JFrame frame = new JFrame();
                    frame.getContentPane().add(transfer);
                    frame.pack();
                    frame.setVisible(true);
                    transfer.start();
                }

                refresh(getSelectionPath());
            } else if (actionCommand.equals("Delete")) {

                if (target != null) {
                    target.delete();

                    // that file is gone so refresh to the parent
                    refresh(getSelectionPath().getParentPath());
                }
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
    }


    //----------------------------------------------------------------------
    // Setters and getters
    //----------------------------------------------------------------------
    /**
     * The (sub-)roots of this JTree. JSRBTree's can display more then one filesystem tree structure.
     *
     * @param   index  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public GeneralFile getRoot(int index) {
        return roots[index];
    }

    /**
     * The (sub-)roots of this JTree. JSRBTree's can display more then one filesystem tree structure.
     *
     * @return  DOCUMENT ME!
     */
    public GeneralFile[] getRoots() {
        return roots;
    }

    /**
     * Returns the MetaDataSelects. The metadata values displayed inline in the tree.
     *
     * @return  DOCUMENT ME!
     */
    public MetaDataSelect[] getSelects() {
        return selects;
    }


    /**
     * Reload the tree model from the filesystem. Clear any stored data. Used by delete and paste to reflect those
     * changes.
     *
     * @param   path  expands the tree at this treepath
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void refresh(TreePath path) throws IOException {

        if (path == null) {
            setSelectionRow(0);
            setModel(new SRBTreeModel(roots, selects));

            return;
        }


        // if file, Get the parent directory, because
        // expandPath() ignores leaf nodes
        if (((GeneralFile) path.getLastPathComponent()).isFile()) {
            path = path.getParentPath();
        }

        setSelectionRow(0);

        //TODO perhaps a refresh should be built into SRBTreeModel
        setModel(new SRBTreeModel(roots, selects));
        expandPath(path);
    }


    /**
     * If true, the default popup menu will be displayed. The event that causes the popup to display is system
     * dependant. Default setting is false.
     *
     * <P>The default menu includes the menu options:<br>
     * Refresh - reloads this tree from the filesystem<br>
     * Query - Opens the query panel using the selected treenode to form the initial query conditions.<br>
     * Copy - Copy the abstract pathname of the selected node into the copyBuffer.<br>
     * Paste - Copy the file pointed to by the copyBuffer to the selected node.<br>
     * Delete - Delete the selected node from the filesystem.</P>
     *
     * @see  MouseEvent.isPopupTrigger()
     */
    public void useDefaultPopupMenu(boolean useDefault) {

        if (!useDefault) {

            // remove popup menu
            popup = null;

            if (popupListener != null) {
                this.removeMouseListener(popupListener);
            }

            return;
        }

        /**
         * Create a popup menu. This menu just adds a few basic actions to the tree.
         */
        popup = new JPopupMenu();

        /*
         *  The optimization code of the SRBTreeModel stores the directory structure locally. This was necessary to
         * reduce the network calls. However, the tree no longer registers changes in the remote filesystem. Requiring
         * we add a refresh option. See also SRBTreeModel.
         */

        /*
        valid windows explorer menu items

        expand
        ---
        search
        ---
        make available offline
        ---
        cut
        copy
        paste
        ---
        delete
        rename
        ---properties
        */

        JMenuItem menuItem = new JMenuItem("Refresh");
        menuItem.addActionListener(this);
        popup.add(menuItem);

        popup.addSeparator();

        menuItem = new JMenuItem("Query");
        menuItem.addActionListener(this);
        popup.add(menuItem);

        popup.addSeparator();

        menuItem = new JMenuItem("Copy");
        menuItem.addActionListener(this);
        popup.add(menuItem);

        menuItem = new JMenuItem("Paste");
        menuItem.addActionListener(this);
        popup.add(menuItem);

        menuItem = new JMenuItem("Delete");
        menuItem.addActionListener(this);
        popup.add(menuItem);


        // Add a listener to this JTree to bring up the popup menu.
        popupListener = new PopupListener();
        this.addMouseListener(popupListener);
    }


    /**
     * Finalizes the object by explicitly letting go of each of its internally held values.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {

        if (popup != null) {
            popup = null;
        }

        if (popupListener != null) {
            popupListener = null;
        }

        if (copyBuffer != null) {
            copyBuffer = null;
        }

        if (roots != null) {
            roots = null;
        }

        if (selects != null) {
            selects = null;
        }

        super.finalize();
    }


    //----------------------------------------------------------------------
    // Example Methods
    //----------------------------------------------------------------------
    /**
     * Initialize this tree to use all the Jargon Tree support classes.
     *
     * @param   roots       DOCUMENT ME!
     * @param   conditions  DOCUMENT ME!
     * @param   selects     DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void createJargonTree(GeneralFile[] roots, MetaDataCondition[] conditions, MetaDataSelect[] selects)
            throws IOException {
        this.roots = roots;
        this.conditions = conditions;
        this.selects = selects;

        /**
         * Load your data into the model.
         */
        setModel(new SRBTreeModel(roots, conditions, selects));


        /**
         * To display the metadata values gathered by the SRBTreeModel selects[], and for other cosmetic improvements,
         * use the JargonTreeCellRenderer with your JTree.
         */
        TreeCellRenderer renderer = new SRBTreeCellRenderer();
        setCellRenderer(renderer);


        /**
         * To edit the metadata values gathered by the SRBTreeModel selects[], use the JargonTreeCellEditor with your
         * JTree.
         */
        setEditable(true);
        setCellEditor(new JargonTreeCellEditor());
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    // ----------------------------------------------------------------------
    // Listeners
    // ----------------------------------------------------------------------
    /**
     * Defines the rather simple event behavior of the default popup menu.
     */
    private class PopupListener extends MouseAdapter {

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void mousePressed(MouseEvent e) {
            triggerPopup(e);
        }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseReleased(MouseEvent e) {
            triggerPopup(e);
        }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        private void triggerPopup(MouseEvent e) {

            if (e.isPopupTrigger()) {

                if (getLastSelectedPathComponent() == null) {
                    return;
                }

                popup.show(e.getComponent(), e.getX(), e.getY());
            }
        }
    }
}
