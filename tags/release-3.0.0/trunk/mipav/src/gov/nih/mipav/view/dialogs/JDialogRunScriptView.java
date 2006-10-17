package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import org.w3c.dom.*;

import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.tree.*;


/**
 * @author   Nathan Pollack -- Contractor (SSAI)
 * @version  0.1 May 24, 2006
 * @see      JDialogRunScriptController
 * @see      JDialogRunScriptModel
 */
public class JDialogRunScriptView implements ActionListener, Observer {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final String SCRIPTNODE = "Script Exectuer";

    /** DOCUMENT ME! */
    private static final String IMAGENODE = "ImageNode";

    /** DOCUMENT ME! */
    private static final String VOINODE = "VioNode";

    /** DOCUMENT ME! */
    private static final String ROOTNODE = "Root";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int centralBuffer;

    /** DOCUMENT ME! */
    private Container contentPane;

    /** DOCUMENT ME! */
    private JDialogRunScriptController controller;

    /** DOCUMENT ME! */
    private JFrame frame;

    /** DOCUMENT ME! */
    private Dimension frame_size;

    /** DOCUMENT ME! */
    private SpringLayout layout;

    /** DOCUMENT ME! */
    private MouseListener listener;

    /** DOCUMENT ME! */
    private DefaultListModel listModel;

    /** DOCUMENT ME! */
    private JDialogRunScriptModel model;

    /** DOCUMENT ME! */
    private int numberOfExecuters = 0;

    /** DOCUMENT ME! */
    private ScriptTreeNode root;

    /** DOCUMENT ME! */
    private Dimension scroll_size = new Dimension();

    /** DOCUMENT ME! */
    private int[] selectedListIndicies;

    /** DOCUMENT ME! */
    private JTree tree = null;

    /** private Dimension initial_frame_size = new Dimension(725,800);. */
    private Dimension tree_size = new Dimension();

    /** DOCUMENT ME! */
    private JScrollPane treeScroll;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogRunScriptView object.
     *
     * @param  controller  DOCUMENT ME!
     * @param  model       DOCUMENT ME!
     */
    public JDialogRunScriptView(JDialogRunScriptController controller, JDialogRunScriptModel model) {
        this.controller = controller;
        this.model = model;

        frame_size = new Dimension(725, 800);
        centralBuffer = (int) (frame_size.getWidth() * 0.015);

        createFrame();
        listener = new DragMouseAdapter();
        displayView();

        update(null, null);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {
        controller.actionPerformed(e);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imagePlaceHolders  DOCUMENT ME!
     * @param  imageLabels        DOCUMENT ME!
     * @param  imageActions       DOCUMENT ME!
     * @param  numberofVOIs       DOCUMENT ME!
     */
    public void addExecuter(String[] imagePlaceHolders, String[] imageLabels, String[] imageActions,
                            int[] numberofVOIs) {
        int lastNodeIndex = ((DefaultTreeModel) tree.getModel()).getChildCount(this.getTreeRoot());
        ScriptTreeNode newNode = createNewExecuter(imagePlaceHolders, imageLabels, imageActions, numberofVOIs);
        ((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, this.getTreeRoot(), lastNodeIndex);
        expandAll(tree, new TreePath(newNode.getPath()), true);
        frame.getContentPane().repaint();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  savedScript  DOCUMENT ME!
     */
    public void createScriptTree(org.w3c.dom.Document savedScript) {
        model.setScriptFile(savedScript.getDocumentElement().getTagName());
        System.out.println("root tag: " + model.getScriptFile());
        this.numberOfExecuters = 0;

        // clean up old tree
        for (int n = root.getChildCount() - 1; n >= 0; n--) {
            ((DefaultTreeModel) tree.getModel()).removeNodeFromParent((ScriptTreeNode) root.getChildAt(n));
        }

        // for each executer restore the nodes under it
        for (int i = 1, j = 0; i < savedScript.getDocumentElement().getChildNodes().getLength(); i += 2, j++) {
            ((DefaultTreeModel) tree.getModel()).insertNodeInto(restoreSavedExecuter(savedScript.getDocumentElement().getChildNodes().item(i)),
                                                                root, j);
            this.numberOfExecuters++;
        }

        expandAll(tree, new TreePath(root.getPath()), true);
        frame.getContentPane().repaint();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imagePlaceHolders  DOCUMENT ME!
     * @param  imageLabels        DOCUMENT ME!
     * @param  imageActions       DOCUMENT ME!
     * @param  numberofVOIs       DOCUMENT ME!
     */
    public void createScriptTree(String[] imagePlaceHolders, String[] imageLabels, String[] imageActions,
                                 int[] numberofVOIs) {
        root = new ScriptTreeNode("root", ROOTNODE);
        tree = new JTree(root);
        root.add(createNewExecuter(imagePlaceHolders, imageLabels, imageActions, numberofVOIs));
        ((DefaultTreeModel) tree.getModel()).reload();
        expandAll(tree, true);
        tree.addMouseListener(listener);
        tree.setDropTarget(new DropJTreeLister());
        tree.setName("Script Tree");
        tree.setCellRenderer(new MyRenderer());
        tree.setRootVisible(false);
        treeScroll = new JScrollPane(tree, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED) {
                public Dimension getPreferredSize() {
                    changeSize();

                    return tree_size;
                }
            };
        treeScroll.setName("Script Tree: scroll");
        contentPane.add(treeScroll);
    }

    /**
     * DOCUMENT ME!
     *
     * @param   name  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public JComponent getComponentByName(String name) {
        JComponent component = null;
        Component[] components = frame.getContentPane().getComponents();

        for (int i = 0; i < components.length; i++) {

            if ((components[i].getName() != null) && (components[i].getName().equalsIgnoreCase(name))) {
                return (JComponent) components[i];
            }
        }

        return component;
    }

    /**
     * ***************************************************************************************************** Various
     * util type functions.*********************************************************************************************
     * ********
     *
     * @return  DOCUMENT ME!
     */
    public JFrame getFrame() {
        return this.frame;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public DefaultListModel getListModel() {
        return this.listModel;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getNumberOfExecuters() {
        return this.numberOfExecuters;
    }

    /**
     * Method to return tree root.
     *
     * @return  DOCUMENT ME!
     */
    public ScriptTreeNode getTreeRoot() {
        return this.root;
    }

    /**
     * Gets the list of images selected by the user in this dialog. Should be in the order that the images are used in
     * the script.
     *
     * @return  A list of images to be used in the script.
     */
    public Vector getUserSelectedImages() {
        Vector scriptExecuters = new Vector(0);

        for (int i = 0; i < root.getChildCount(); i++) {
            Vector imageNames = new Vector(0);

            for (int j = 0; j < ((TreeNode) root.getChildAt(i)).getChildCount(); j++) {
                String imageName = ((TreeNode) root.getChildAt(i).getChildAt(j)).toString().trim();
                imageNames.add(imageName);
            }

            scriptExecuters.add(imageNames);
        }

        return scriptExecuters;
    }

    /**
     * Gets the list of VOIs selected by the user in this dialog. Should be in the order that the VOIs are used in the
     * script.
     *
     * @return  A list of VOIs to be used in the script.
     */
    public Vector getUserSelectedVOIs() {
        Vector scriptExecuters = new Vector(0);

        for (int i = 0; i < root.getChildCount(); i++) {
            Vector voiPaths = new Vector(0);

            for (int j = 0; j < ((TreeNode) root.getChildAt(i)).getChildCount(); j++) {

                for (int k = 0; k < ((TreeNode) root.getChildAt(i).getChildAt(j)).getChildCount(); k++) {
                    String voi = ((TreeNode) root.getChildAt(i).getChildAt(j).getChildAt(k)).toString().trim();
                    voiPaths.add(voi);
                }
            }

            scriptExecuters.add(voiPaths);
        }

        return scriptExecuters;
    }

    /**
     * ***************************************************************************************************** Called when
     * there is a change to the Model.**********************************************************************************
     * *******************
     *
     * @param  o    DOCUMENT ME!
     * @param  arg  DOCUMENT ME!
     */
    public void update(Observable o, Object arg) {

        if ((JScrollPane) getComponentByName("Images List: scroll") == null) {
            return;
        }

        JList imageList = ((JList) ((JScrollPane) getComponentByName("Images List: scroll")).getViewport().getView());
        imageList.setModel(populateModel(model.getAvailableImageList().toArray()));

        if (imageList.getSelectedIndex() == -1) {
            imageList.setSelectedIndex(0);
        }

        JList voiList = ((JList)
                             ((JScrollPane) getComponentByName("VOIs from above image List: scroll")).getViewport().getView());

        if ((model.getAvailableImageList() != null) && (model.getAvailableImageList().size() > 0)) {
            voiList.setListData((((ScriptImage) imageList.getSelectedValue()).getScriptVOIs()));
        }

        frame.toFront();
        frame.repaint();
    }

    /**
     * Checks tree to see that all place holders have been replaced with actual images and VOIs if not, it prompts the
     * user with a warning message, and highlights the first node that still contains a placeholer.
     *
     * @return  <code>True</code> if all of the image and voi placeholders in all script executors have had images/vois
     *          assigned to them, <code>false</code> otherwise.
     */
    protected boolean isTreeReadyForScriptExecution() {
        Enumeration bf = root.breadthFirstEnumeration();

        while (bf.hasMoreElements()) {
            ScriptTreeNode node = node = (ScriptTreeNode) bf.nextElement();

            if ((node.toString().startsWith("$")) || (node.toString().indexOf("VOI Needed") != -1)) {
                tree.addSelectionPath(new TreePath(node.getPath()));
                MipavUtil.displayWarning("Image or VOI placeholders still found.  Script execution aborted.");

                return false;
            }
        }

        return true;
    }

    /**
     * Scroll List Code.
     *
     * @param   contents  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static DefaultListModel populateModel(Object[] contents) {
        DefaultListModel listModel = new DefaultListModel();

        if ((contents != null) && (contents.length > 0)) {

            for (int i = 0; i < contents.length; i++) {
                listModel.add(i, contents[i]);
            }
        }

        return listModel;
    }

    /**
     * ***************************************************************************************************** Creating
     * buttons.*****************************************************************************************************
     *
     * @param  buttonName  DOCUMENT ME!
     */
    private void addButton(String buttonName) {
        JButton newButton = new JButton(buttonName);
        newButton.setName(buttonName);
        newButton.setActionCommand(buttonName);
        newButton.addActionListener(this);
        contentPane.add(newButton);
    }

    /**
     * ***************************************************************************************************** Creating
     * labels.*****************************************************************************************************
     *
     * @param  labelName  DOCUMENT ME!
     */
    private void addLabel(String labelName) {
        JLabel newLabel = new JLabel(labelName);
        newLabel.setName(labelName);
        newLabel.setFont(new Font(null, Font.BOLD, 12));
        contentPane.add(newLabel);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  menuItems  DOCUMENT ME!
     */
    private void addMenu(String[] menuItems) {
        JMenuBar menuBar = new JMenuBar();
        JMenu menu = new JMenu(menuItems[0]);
        menuBar.add(menu);

        for (int i = 1; i < menuItems.length; i++) {
            JMenuItem item = new JMenuItem(menuItems[i]);
            item.setActionCommand(menuItems[i]);
            item.addActionListener(this);
            menu.add(item);
        }

        frame.setJMenuBar(menuBar);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  name  DOCUMENT ME!
     */
    private void addScrollList(String name) {
        Object[] contents = null;
        listModel = new DefaultListModel();

        if (name.equalsIgnoreCase("Images List")) {
            contents = model.getAvailableImageList().toArray();
        }

        if (name.equalsIgnoreCase("VOIs from above image List")) {

            if ((model.getAvailableImageList() != null) && (model.getAvailableImageList().size() > 0)) {
                contents = ((ScriptImage) model.getAvailableImageList().get(0)).getScriptVOIs();
            }
        }

        populateModel(contents);

        JList list = new JList(listModel);
        list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        list.setDragEnabled(true);
        list.addMouseListener(listener);
        list.setName(name);
        list.setSelectedIndex(0);

        JScrollPane scroll = new JScrollPane(list, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                             JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED) {
            public Dimension getPreferredSize() {
                changeSize();

                return scroll_size;
            }
        };
        scroll.setName(name + ": scroll");
        frame.getContentPane().add(scroll);
    }

    /**
     * TODO This no longer works, need to update how it determines which node it is.
     *
     * @param  selectedNode  DOCUMENT ME!
     */
    private void applyToAllNodes(ScriptTreeNode selectedNode) {
        int[] index = new int[selectedNode.getPath().length];

        for (int i = 0; i < (selectedNode.getPath().length - 1); i++) {
            index[i] = ((DefaultTreeModel) tree.getModel()).getIndexOfChild(selectedNode.getPath()[i],
                                                                            selectedNode.getPath()[i + 1]);
        }

        ScriptTreeNode[] copyNodes = new ScriptTreeNode[numberOfExecuters];

        for (int j = 0; j < numberOfExecuters; j++) {
            ScriptTreeNode node = ((ScriptTreeNode)
                                       ((ScriptTreeNode) ((DefaultTreeModel) tree.getModel()).getRoot()).getChildAt(j).getChildAt(index[1]));
            String selectNodeName = (String) selectedNode.getUserObject();

            if (selectedNode.getNodeType().equalsIgnoreCase(this.IMAGENODE)) {
                copyNodes[j] = node;
            } else if (selectedNode.getNodeType().equalsIgnoreCase(this.VOINODE)) {
                copyNodes[j] = (ScriptTreeNode) node.getChildAt(index[2]);
            }

            updateNode(copyNodes[j], selectNodeName);
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void changeSize() {
        frame_size = frame.getSize();
        tree_size.setSize((frame_size.getWidth() * 0.6), (frame_size.getHeight() * 0.85));
        scroll_size.setSize((frame_size.getWidth() * 0.35), (frame_size.getHeight() * 0.39));
        centralBuffer = (int) (frame.getSize().getWidth() * 0.015);
    }

    /**
     * DOCUMENT ME!
     */
    private void createFrame() {

        // UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        frame = new JFrame() {
                public Dimension getPreferredSize() {
                    changeSize();

                    return frame_size;
                }
            };
        frame.setSize(frame_size);
        frame.setTitle("MIPAV Script Tool: " + model.getScriptFile());
        frame.setIconImage(MipavUtil.getIcon("gear.gif").getImage());
        contentPane = frame.getContentPane();
        layout = new SpringLayout();
        contentPane.setLayout(layout);
    }

    /**
     * DOCUMENT ME!
     *
     * @param   imagePlaceHolders  DOCUMENT ME!
     * @param   imageLabels        DOCUMENT ME!
     * @param   imageActions       DOCUMENT ME!
     * @param   numberofVOIs       DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private ScriptTreeNode createNewExecuter(String[] imagePlaceHolders, String[] imageLabels, String[] imageActions,
                                             int[] numberofVOIs) {
        ScriptTreeNode newNode = new ScriptTreeNode("Script Executer", this.SCRIPTNODE);
        this.numberOfExecuters++;

        ScriptTreeNode[] imageNodes;
        ScriptTreeNode voi = null;
        imageNodes = new ScriptTreeNode[imagePlaceHolders.length];

        for (int i = 0; i < imagePlaceHolders.length; i++) {
            imageNodes[i] = new ScriptTreeNode(imagePlaceHolders[i] + " (" + imageActions[i] + " -- " + imageLabels[i] +
                                               ")", this.IMAGENODE);
            newNode.add(imageNodes[i]);

            for (int j = 0; j < numberofVOIs[i]; j++) {
                voi = new ScriptTreeNode("VOI Needed", this.VOINODE);
                imageNodes[i].add(voi);
            }
        }

        return newNode;
    }

    /**
     * DOCUMENT ME!
     */
    private void displayView() {
        createScriptTree(model.getScriptImageVars(), model.getScriptImageLabels(), model.getScriptImageActions(),
                         model.getNumberOfRequiredVOIsForScriptImages());

        String[] listName = { "Images List", "VOIs from above image List" };

        for (int a = 0; a < listName.length; a++) {
            addScrollList(listName[a]);
        }

        String[] labelNames = { "Script Execution Setup", "Images", "VOIs from selected image" }; // ,"VOIs from File"};

        for (int b = 0; b < labelNames.length; b++) {
            addLabel(labelNames[b]);
        }

        String[] buttonNames = { "Run Script", "Add new script executer", "Add image from file", "Add VOI from file" };

        for (int c = 0; c < buttonNames.length; c++) {
            addButton(buttonNames[c]);
        }

        String[] menuItems = {
            "File", "Open saved image and VOI selections", "Save current image and VOI selections",
            "View current script contents", "Close"
        };
        addMenu(menuItems);

        for (int i = 0; i < frame.getContentPane().getComponents().length; i++) {
            setLayoutConstraints(frame.getContentPane().getComponents()[i]);
        }

        frame.pack();
        frame.setSize(frame_size);
        MipavUtil.centerOnScreen(frame);
        frame.setVisible(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tree    DOCUMENT ME!
     * @param  expand  DOCUMENT ME!
     */
    private void expandAll(JTree tree, boolean expand) {
        TreeNode root = (TreeNode) tree.getModel().getRoot();
        expandAll(tree, new TreePath(root), expand);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tree    DOCUMENT ME!
     * @param  parent  DOCUMENT ME!
     * @param  expand  DOCUMENT ME!
     */
    private void expandAll(JTree tree, TreePath parent, boolean expand) {
        TreeNode node = (TreeNode) parent.getLastPathComponent();

        if (node.getChildCount() >= 0) {

            for (Enumeration e = node.children(); e.hasMoreElements();) {
                TreeNode n = (TreeNode) e.nextElement();
                TreePath path = parent.pathByAddingChild(n);
                expandAll(tree, path, expand);
            }
        }

        if (expand) {
            tree.expandPath(parent);
        } else {
            tree.collapsePath(parent);
        }
    }

    /**
     * Called when a script is loaded from disk.
     *
     * @param   executer  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private ScriptTreeNode restoreSavedExecuter(Node executer) {
        System.out.println("executer: " + executer.getNodeName());

        ScriptTreeNode newNode = new ScriptTreeNode("Script Executer", this.SCRIPTNODE);
        ScriptTreeNode[] imageNodes = new ScriptTreeNode[executer.getChildNodes().getLength()];
        ScriptTreeNode voi = null;
        System.out.println("imageNodes length: " + imageNodes.length);
        System.out.println("first Child: " + executer.getFirstChild().getNodeName());

        // loop over images
        for (int i = 1; i < executer.getChildNodes().getLength(); i += 2) {
            int ii = 0;
            imageNodes[ii] = new ScriptTreeNode(executer.getChildNodes().item(i).getAttributes().getNamedItem("name").getNodeValue(),
                                                this.IMAGENODE);
            imageNodes[ii].setDefaultName(executer.getChildNodes().item(i).getAttributes().getNamedItem("defaultName").getNodeValue());
            imageNodes[ii].setFilePath(executer.getChildNodes().item(i).getAttributes().getNamedItem("filePath").getNodeValue());

            // imageNodes[ii] = new
            // ScriptTreeNode(executer.getChildNodes().item(i).getNodeName().replace("__",
            // " "), this.IMAGENODE);
            newNode.add(imageNodes[ii]);
            System.out.println("newNode = " + imageNodes[ii]);

            for (int j = 1; j < executer.getChildNodes().item(i).getChildNodes().getLength(); j += 2) {
                voi = new ScriptTreeNode(executer.getChildNodes().item(i).getChildNodes().item(j).getNodeName().replaceAll("__",
                                                                                                                           " "),
                                         this.VOINODE);
                imageNodes[ii].add(voi);
            }

            ii++;
        }

        return newNode;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  component  DOCUMENT ME!
     */
    private void setLayoutConstraints(Component component) {

        if (component.getName().equalsIgnoreCase("Script Execution Setup")) {
            setSprings(component, contentPane, contentPane, 5, 5);
        } else if (component.getName().equalsIgnoreCase("Images")) {
            setSprings(component, getComponentByName("Script Execution Setup"),
                       getComponentByName("Images List: scroll"), 0, 0);
        } else if (component.getName().equalsIgnoreCase("VOIs from selected image")) {
            layout.putConstraint(SpringLayout.NORTH, component, 5, SpringLayout.SOUTH,
                                 getComponentByName("Add image from file"));
            layout.putConstraint(SpringLayout.WEST, component, 0, SpringLayout.WEST,
                                 getComponentByName("Images List: scroll"));
        } else if (component.getName().equalsIgnoreCase("Script Tree: scroll")) {
            setSprings(component, contentPane, contentPane, 20, 5);
        } else if (component.getName().equalsIgnoreCase("Run Script")) {
            layout.putConstraint(SpringLayout.NORTH, component, 5, SpringLayout.SOUTH,
                                 getComponentByName("Script Tree: scroll"));
            layout.putConstraint(SpringLayout.WEST, component, 0, SpringLayout.WEST,
                                 getComponentByName("Script Tree: scroll"));
        } else if (component.getName().equalsIgnoreCase("Add new script executer")) {
            layout.putConstraint(SpringLayout.NORTH, component, 0, SpringLayout.NORTH,
                                 getComponentByName("Run Script"));
            layout.putConstraint(SpringLayout.WEST, component, 5, SpringLayout.EAST, getComponentByName("Run Script"));
        } else if (component.getName().equalsIgnoreCase("Add image from file")) {
            layout.putConstraint(SpringLayout.NORTH, component, 5, SpringLayout.SOUTH,
                                 getComponentByName("Images List: scroll"));
            layout.putConstraint(SpringLayout.WEST, component, 0, SpringLayout.WEST,
                                 getComponentByName("Images List: scroll"));
        } else if (component.getName().equalsIgnoreCase("Add VOI from file")) {
            layout.putConstraint(SpringLayout.NORTH, component, 0, SpringLayout.NORTH,
                                 getComponentByName("Run Script"));
            layout.putConstraint(SpringLayout.WEST, component, centralBuffer, SpringLayout.EAST,
                                 getComponentByName("Script Tree: scroll"));
        } else if (component.getName().equalsIgnoreCase("Images List: scroll")) {
            layout.putConstraint(SpringLayout.NORTH, component, 0, SpringLayout.NORTH,
                                 getComponentByName("Script Tree: scroll"));
            layout.putConstraint(SpringLayout.WEST, component, centralBuffer, SpringLayout.EAST,
                                 getComponentByName("Script Tree: scroll"));
        } else if (component.getName().equalsIgnoreCase("VOI List: scroll")) {
            layout.putConstraint(SpringLayout.SOUTH, component, 0, SpringLayout.SOUTH,
                                 getComponentByName("Script Tree: scroll"));
            layout.putConstraint(SpringLayout.NORTH, component, 20, SpringLayout.SOUTH,
                                 getComponentByName("VOIs from above image List: scroll"));
            layout.putConstraint(SpringLayout.WEST, component, 0, SpringLayout.WEST,
                                 getComponentByName("Images List: scroll"));
        } else if (component.getName().equalsIgnoreCase("VOIs from above image List: scroll")) {
            layout.putConstraint(SpringLayout.NORTH, component, 20, SpringLayout.SOUTH,
                                 getComponentByName("Add image from file"));
            layout.putConstraint(SpringLayout.WEST, component, 0, SpringLayout.WEST,
                                 getComponentByName("Add image from file"));
        } else if (component.getName().equalsIgnoreCase("VOIs from File")) {
            layout.putConstraint(SpringLayout.NORTH, component, 5, SpringLayout.SOUTH,
                                 getComponentByName("VOIs from above image List: scroll"));
            layout.putConstraint(SpringLayout.WEST, component, 0, SpringLayout.WEST,
                                 getComponentByName("Add image from file"));
        }
    }

    /**
     * ***************************************************************************************************** Setting
     * layout constraints.**********************************************************************************************
     * *******
     *
     * @param  component    DOCUMENT ME!
     * @param  northAnchor  DOCUMENT ME!
     * @param  westAnchor   DOCUMENT ME!
     * @param  northSpring  DOCUMENT ME!
     * @param  westSprint   DOCUMENT ME!
     */
    private void setSprings(Component component, Component northAnchor, Component westAnchor, int northSpring,
                            int westSprint) {
        layout.putConstraint(SpringLayout.NORTH, component, northSpring, SpringLayout.NORTH, northAnchor);
        layout.putConstraint(SpringLayout.WEST, component, westSprint, SpringLayout.WEST, westAnchor);
    }

    /**
     * ***************************************************************************************************** Methods to
     * handle tree behavior*********************************************************************************************
     * ******** updateNode, called when a node text needs to be changed.
     *
     * @param  node  DOCUMENT ME!
     * @param  text  DOCUMENT ME!
     */
    private void updateNode(ScriptTreeNode node, String text) {
        ScriptTreeNode newNode = new ScriptTreeNode(text, node.getNodeType());
        ScriptTreeNode parentNode = (ScriptTreeNode) node.getParent();

        while (node.children().hasMoreElements()) {
            newNode.add((ScriptTreeNode) node.children().nextElement());
        }

        newNode.setDefaultName(node.getDefaultName());
        ((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, parentNode, parentNode.getIndex(node));
        ((DefaultTreeModel) tree.getModel()).removeNodeFromParent(node);
        expandAll(tree, new TreePath(newNode.getPath()), true);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    private class DragMouseAdapter extends MouseAdapter {

        /** DOCUMENT ME! */
        ScriptTreeNode parentNode;

        /** DOCUMENT ME! */
        ScriptTreeNode resetNode;

        /** DOCUMENT ME! */
        ScriptTreeNode selectedNode;

        /** DOCUMENT ME! */
        String selectedNodeName;

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void mousePressed(MouseEvent e) {

            if (((Component) e.getSource()).getName().equalsIgnoreCase("Images List")) {
                selectedListIndicies = ((JList) e.getSource()).getSelectedIndices();

                if ((selectedListIndicies.length > 1) && (e.getModifiers() != MouseEvent.BUTTON3_MASK)) {
                    return;
                }

                if ((selectedListIndicies.length > 1) && (e.getModifiers() == MouseEvent.BUTTON3_MASK)) {

                    if (root.getChildAt(0).getChildCount() == 1) {
                        Point pt = e.getPoint();
                        JPopupMenu popup = new JPopupMenu();
                        JMenuItem sequentialMenuItem = new JMenuItem("Add Images Sequentially to Executers");
                        sequentialMenuItem.addActionListener(new java.awt.event.ActionListener() {
                                public void actionPerformed(java.awt.event.ActionEvent e) {

                                    if (selectedListIndicies.length > numberOfExecuters) {
                                        int additionalExecuters = selectedListIndicies.length - numberOfExecuters;

                                        for (int i = 0; i < additionalExecuters; i++) {
                                            controller.actionPerformed(new ActionEvent(this, 0,
                                                                                       "Add new script executer"));
                                        }
                                    }

                                    // get first image node
                                    for (int j = 0; j < selectedListIndicies.length; j++) {
                                        String imageName = ((JList)
                                                                ((JScrollPane) getComponentByName("Images List: scroll"))
                                                                    .getViewport().getView()).getModel().getElementAt(j).toString();
                                        updateNode((ScriptTreeNode) root.getChildAt(j).getChildAt(0), imageName);
                                    }
                                }
                            }); // addActionListener

                        popup.add(sequentialMenuItem);
                        popup.show((Component) e.getSource(), pt.x + 10, pt.y + 10);

                        return;
                    } // if only one image
                } // more than one image selected, and button 3

                ScriptImage image = (ScriptImage) ((JList) e.getSource()).getSelectedValue();
                JList voiList = null;
                Component[] components = frame.getContentPane().getComponents();

                for (int i = 0; i < frame.getContentPane().getComponents().length; i++) {

                    if ((components[i].getName() != null) &&
                            (components[i].getName().equalsIgnoreCase("VOIs from above image List: scroll"))) {
                        voiList = ((JList)
                                       ((JScrollPane) frame.getContentPane().getComponents()[i]).getViewport().getView());

                        // voiList.setListData(image.getModelImage().getVOIs());
                        voiList.setListData(image.getScriptVOIs());
                    } // if
                } // for
            } // source equals imageList

            if (((Component) e.getSource()).getName().equalsIgnoreCase("Script Tree")) {

                if (e.getModifiers() == InputEvent.BUTTON3_MASK) {
                    Point pt = e.getPoint();
                    TreePath pathTarget = tree.getPathForLocation(pt.x, pt.y);
                    selectedNode = (ScriptTreeNode) pathTarget.getLastPathComponent();
                    parentNode = (ScriptTreeNode) selectedNode.getParent();
                    selectedNodeName = (String) selectedNode.getUserObject();

                    if (((String) selectedNode.getUserObject()).indexOf("$Image") != -1) {
                        return;
                    }

                    if (((String) selectedNode.getUserObject()).indexOf("VOI Needed") != -1) {
                        return;
                    }

                    JMenuItem deleteMenuItem = new JMenuItem("Delete");
                    deleteMenuItem.addActionListener(new java.awt.event.ActionListener() {
                            public void actionPerformed(java.awt.event.ActionEvent e) {

                                if ((selectedNodeName.indexOf("Script") != -1)) {
                                    ((DefaultTreeModel) tree.getModel()).removeNodeFromParent(selectedNode);
                                    numberOfExecuters--;

                                    return;
                                } else {
                                    resetNode = new ScriptTreeNode(selectedNode.getDefaultName(),
                                                                   selectedNode.getNodeType());
                                }

                                while (selectedNode.children().hasMoreElements()) {
                                    resetNode.add((ScriptTreeNode) selectedNode.children().nextElement());
                                }

                                ((DefaultTreeModel) tree.getModel()).insertNodeInto(resetNode, parentNode,
                                                                                    parentNode.getIndex(selectedNode));
                                ((DefaultTreeModel) tree.getModel()).removeNodeFromParent(selectedNode);
                                expandAll(tree, new TreePath(resetNode.getPath()), true);
                            }
                        } // new ActionListener
                                                    ); // addActionListener

                    // TODO: Apply to all script executers does not work currently.  null pointer exception on first
                    // line of updateNode()

                    // JMenuItem applyAllMenuItem = new JMenuItem("Apply to all script executers");
                    // applyAllMenuItem.addActionListener(new java.awt.event.ActionListener() {        public void
                    // actionPerformed(java.awt.event.ActionEvent e) {            applyToAllNodes(selectedNode);
                    // } // actionPerformed    } // new ActionListener                                  ); //
                    // addActionListener

                    JPopupMenu popup = new JPopupMenu();
                    popup.add(deleteMenuItem);

                    // popup.add(applyAllMenuItem);
                    popup.show(tree, pt.x + 10, pt.y + 10);
                } // right mouse button clicked

                return;
            } // source equals scriptTree

            JComponent c = (JComponent) e.getSource();
            TransferHandler handler = c.getTransferHandler();

            if (selectedListIndicies.length == 1) {
                handler.exportAsDrag(c, e, TransferHandler.COPY);
            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    private class DropJTreeLister extends DropTarget {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -3115134852321228978L;

        /**
         * DOCUMENT ME!
         *
         * @param  dtde  DOCUMENT ME!
         */
        public void drop(DropTargetDropEvent dtde) {
            Transferable transferable = dtde.getTransferable();
            DataFlavor[] flavors = transferable.getTransferDataFlavors();
            DataFlavor dataFlavor = null;

            /*
             * Finds the data flavor for transfering plain text, uses that to retreive the text of the image/voi being
             * transfered
             */
            for (int k = 0; k < flavors.length; k++) {

                if (flavors[k].isMimeTypeEqual("text/plain") && (flavors[k].isRepresentationClassSerializable())) {
                    dataFlavor = flavors[k];
                }
            }

            String text = null;

            try {
                text = (String) transferable.getTransferData(dataFlavor);
            } catch (java.io.IOException ioe) {
                ioe.printStackTrace();
            } catch (UnsupportedFlavorException ufe) {
                ufe.printStackTrace();
            }

            int action = dtde.getDropAction();
            Point pt = dtde.getLocation();
            TreePath pathTarget = tree.getPathForLocation(pt.x, pt.y);

            if (pathTarget == null) {
                return;
            }

            ScriptTreeNode oldNode = (ScriptTreeNode) pathTarget.getLastPathComponent();
            String oldNodeName = (String) oldNode.getUserObject();

            if ((oldNodeName.indexOf("Script") != -1)) {
                return;
            }

            ScriptTreeNode newNode = new ScriptTreeNode(text, oldNode.getNodeType());
            ScriptTreeNode parentNode = (ScriptTreeNode) oldNode.getParent();

            /*
             * If the drop target is a VOI Node, checks if the parent node is equal to the selected node, if not then
             * this VOI does not go with this image, a warning message is shown, and no action if performed
             */
            if (oldNode.getNodeType().equalsIgnoreCase(VOINODE)) {

                if (!(((JList) ((JScrollPane) getComponentByName("Images List: scroll")).getViewport().getView())
                          .getSelectedValue().toString().equalsIgnoreCase(parentNode.getUserObject().toString()))) {
                    MipavUtil.displayWarning("VOIs must be from same image.");

                    return;
                }
            }

            if ((oldNode.getNodeType().equalsIgnoreCase(IMAGENODE)) &&
                    (oldNode.getChildCount() ==
                         ((JList)
                                  ((JScrollPane) getComponentByName("VOIs from above image List: scroll")).getViewport().getView())
                         .getModel().getSize())) {

                for (int i = 0; i < oldNode.getChildCount(); i++) {
                    String voiName = ((JList)
                                          ((JScrollPane) getComponentByName("VOIs from above image List: scroll"))
                                              .getViewport().getView()).getModel().getElementAt(i).toString();
                    ScriptTreeNode voiChildNode = new ScriptTreeNode(voiName, VOINODE);
                    voiChildNode.setDefaultName("VOI Needed");
                    newNode.add(voiChildNode);
                } // for
            } else {

                while (oldNode.children().hasMoreElements()) {
                    newNode.add((ScriptTreeNode) oldNode.children().nextElement());
                }
            }

            newNode.setDefaultName(oldNode.getDefaultName());
            ((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, parentNode, parentNode.getIndex(oldNode));
            ((DefaultTreeModel) tree.getModel()).removeNodeFromParent(oldNode);
            expandAll(tree, new TreePath(newNode.getPath()), true);
            dtde.acceptDrop(action);
            dtde.dropComplete(true);
        }
    }

    /**
     * ***************************************************************************************************** Inner
     * classes to follow************************************************************************************************
     * ***** protected class ModelImageForScripting{ ModelImage modelImage; ModelImageForScripting(ModelImage
     * modelImage){ this.modelImage = modelImage; } public ModelImage getModelImage(){ return this.modelImage; } public
     * String toString(){ return modelImage.getImageName(); } }.
     */
    private class MyRenderer extends DefaultTreeCellRenderer {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 5484058426833940837L;

        /** DOCUMENT ME! */
        Icon imageIcon, voiIcon, scriptIcon;

        /**
         * Creates a new MyRenderer object.
         */
        public MyRenderer() {
            this.imageIcon = MipavUtil.getIcon("cube.gif");
            this.voiIcon = MipavUtil.getIcon("polygon.gif");
            this.scriptIcon = MipavUtil.getIcon("script.gif");
        }

        /**
         * DOCUMENT ME!
         *
         * @param   tree      DOCUMENT ME!
         * @param   value     DOCUMENT ME!
         * @param   sel       DOCUMENT ME!
         * @param   expanded  DOCUMENT ME!
         * @param   leaf      DOCUMENT ME!
         * @param   row       DOCUMENT ME!
         * @param   hasFocus  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded,
                                                      boolean leaf, int row, boolean hasFocus) {
            super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);

            ScriptTreeNode node = (ScriptTreeNode) value;

            if (node.getUserObject().toString().equalsIgnoreCase(node.getDefaultName())) {
                this.setForeground(Color.GRAY);
            }

            if ((node.getUserObject().toString().indexOf("$Image") != -1) ||
                    (node.getUserObject().toString().indexOf("$image") != -1)) {
                setIcon(imageIcon);
            } else if (model.isImage(node.getUserObject().toString())) {
                setIcon(imageIcon);
            } else if (!(node.isRoot()) && (((ScriptTreeNode) node.getParent()).isRoot())) {
                setIcon(scriptIcon);
                this.setForeground(Color.BLACK);
            } else {
                setIcon(voiIcon);
            }

            return this;
        }
    }
} // class
