package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import org.w3c.dom.*;

import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;


/**
 * @author   Nathan Pollack -- Contractor (SSAI)
 * @version  0.1 May 24, 2006
 * @see      JDialogRunScriptController
 * @see      JDialogRunScriptModel
 */
public class JDialogRunScriptView implements ActionListener, ListSelectionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Used for XML tag for an image placeholder. */
    private static final String PLACEHOLDER_STR = "Image_Placeholder";

    /** Used for XML tag for an image. */
    private static final String IMAGE_STR = "Image";

    /** Used for XML tag for a VOI. */

    /** Script node type (Script)... only one and is only child of root */
    private static final int SCRIPTNODE = 0;

    /** Image placeholder node type never created nor deleted by user. */
    private static final int IMAGEPLACEHOLDERNODE = 1;

    /** Image node type, falls under an imageplaceholder node, created and deleted by user. */
    private static final int IMAGENODE = 2;

    /** VOI node type, begins with VOI_EMPTY string and must be set if present before running of the script. */
    private static final int VOINODE = 3;

    /** Root node type. */
    private static final int ROOTNODE = 4;

    /** DOCUMENT ME! */
    private static final int IMAGE_DROP = 0;

    /** DOCUMENT ME! */
    private static final int VOI_DROP = 1;

    /** DOCUMENT ME! */
    //private static final int TREE_DROP = 2;

    /** DOCUMENT ME! */
    private static final String VOI_EMPTY = "[Insert VOI]";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int centralBuffer;

    /** DOCUMENT ME! */
    private Container contentPane;

    /** DOCUMENT ME! */
    private JDialogRunScriptController controller;

    /** DOCUMENT ME! */
    private int dropSource = 0;

    /** DOCUMENT ME! */
    private Vector<Object> emptyVector = new Vector<Object>();

    /** DOCUMENT ME! */
    private JFrame frame;

    /** DOCUMENT ME! */
    private Dimension frame_size;

    /** DOCUMENT ME! */
    private JList imageList = null;

    /** DOCUMENT ME! */
    private SpringLayout layout;

    /** Menu for setting raw info*/
    private JPopupMenu popup;

    /** Listener to be added/removed depending on selection status*/
    private PopupListener popupListener;
    
    /** DOCUMENT ME! */
    private JDialogRunScriptModel model;

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

    /** Keep a reference to the VOI List to turn on and off if multiple images are selected. */
    private JList voiList = null;
    
    /** child count for script node **/
    private int scriptNodeChildCount = -1;

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
        displayView();
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

    public void valueChanged(ListSelectionEvent e) {
    	int numSel = imageList.getSelectedIndices().length;
    	Object [] sel = imageList.getSelectedValues();
    	int numOpenedByScript = 0;
    	for (int i = 0; i < numSel; i++) {
    		if (((ScriptImage)sel[i]).isOpenedByScript()) {
    			numOpenedByScript++;
    		}
    	}
    	
    	if (numOpenedByScript < 1) {
    		if (popupListener != null) {
    			imageList.removeMouseListener(popupListener);
    		}
    		return;
    	}
    	
    	if (popup == null) {
    		popup = new JPopupMenu();
    		JMenuItem menuItem = new JMenuItem("Set RAW opening information");
            menuItem.setActionCommand("RawInfo");
            menuItem.addActionListener(this);
            popup.add(menuItem);
            popupListener = new PopupListener();
            imageList.addMouseListener(popupListener);
    	} else {
    		imageList.addMouseListener(popupListener);
    	}
    	
    	
        
    	
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  savedScript  DOCUMENT ME!
     */
    public void createScriptTree(org.w3c.dom.Document savedScript) {

        ScriptTreeNode scriptNode = (ScriptTreeNode) root.getChildAt(0);


        int numImagePlaceHolders = scriptNode.getChildCount();

        ScriptTreeNode imagePHNode = null;
        int numImages;
        DefaultTreeModel tModel = (DefaultTreeModel) tree.getModel();

        // clean up old tree
        for (int i = 0; i < numImagePlaceHolders; i++) {
            imagePHNode = (ScriptTreeNode) scriptNode.getChildAt(i);
            numImages = imagePHNode.getChildCount();

            for (int j = numImages - 1; j >= 0; j--) {
                tModel.removeNodeFromParent((ScriptTreeNode) imagePHNode.getChildAt(j));
            }
        }

        int numDocPlaceHolders = savedScript.getDocumentElement().getChildNodes().getLength();
        Node elementPH;
        Node elementImage;
        Node elementVOI;
        int numDocImages;

        String imageName;
        String imageFilePath;
        String voiName;
        String voiPath;

        int phIndex = 0;
        int voiCount = 0;
        ScriptTreeNode newNode;
        ScriptTreeNode targetNode;
        ScriptTreeNode voiNode;
        int childCount;
        boolean isMulti;
        NodeList children = savedScript.getDocumentElement().getChildNodes();

        // run through once and see if
        int phCount = 0;

        for (int i = 0; i < numDocPlaceHolders; i++) {
            elementPH = (Node) children.item(i);

            if ((elementPH.getNodeType() == Node.ELEMENT_NODE) && elementPH.getNodeName().equals(PLACEHOLDER_STR)) {
                phCount++;
            }
        }

        if (phCount != numImagePlaceHolders) {
            MipavUtil.displayError("Script's image placeholders do not match saved image placeholders");

            return;
        }


        for (int i = 0; i < numDocPlaceHolders; i++) {

            elementPH = (Node) children.item(i);

            if ((elementPH.getNodeType() == Node.ELEMENT_NODE) && elementPH.getNodeName().equals(PLACEHOLDER_STR)) {
                numDocImages = elementPH.getChildNodes().getLength();

                for (int j = 0; j < numDocImages; j++) {
                    elementImage = (Node) elementPH.getChildNodes().item(j);

                    if ((elementImage.getNodeType() == Node.ELEMENT_NODE) &&
                            elementImage.getNodeName().equals(IMAGE_STR)) {
                        imageName = elementImage.getAttributes().getNamedItem("name").getNodeValue();
                        imageFilePath = elementImage.getAttributes().getNamedItem("filePath").getNodeValue();
                        isMulti = Boolean.valueOf(elementImage.getAttributes().getNamedItem("isMulti").getNodeValue()).booleanValue();

                        targetNode = (ScriptTreeNode) scriptNode.getChildAt(phIndex);
                        childCount = targetNode.getChildCount();
                        newNode = new ScriptTreeNode(imageName, IMAGENODE);
                        newNode.setFilePath(imageFilePath);
                        // inserting into placeholder as new image node
                        // look for last image child and append after that

                        // check to see if the image is currently open, if not, add to list of Images on right
                        if (model.getScriptImage(imageName) == null) {

                            // not sure about the isMulti setting to true always...
                            model.addToAvailableImageList(imageName, imageFilePath, isMulti);
                        }

                        int imageIndex = model.getImageIndex(imageName);

                        // first add all required VOIs
                        voiCount = model.getNumberOfRequiredVOIsForScriptImages()[phIndex];
                        System.err.println("VOI COUNT IS: " + voiCount);

                        for (int k = 0; k < elementImage.getChildNodes().getLength(); k++) {
                            elementVOI = elementImage.getChildNodes().item(k);

                            if (elementVOI.getNodeType() == Node.ELEMENT_NODE) {
                                voiName = elementVOI.getAttributes().getNamedItem("name").getNodeValue();
                                voiPath = elementVOI.getAttributes().getNamedItem("filePath").getNodeValue();

                                if (model.getScriptImage(imageName).getScriptVOI(voiName) == null) {
                                    model.addVOI(voiName, voiPath, imageIndex);
                                }

                                voiNode = new ScriptTreeNode(elementVOI.getAttributes().getNamedItem("name").getNodeValue(),
                                                             VOINODE);
                                newNode.add(voiNode);
                            }
                        }

                        ((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, targetNode, childCount);


                    }


                    // System.err.println("\t" + elementImage.getNodeName() + ", value: " + elementImage.getNodeValue()
                    // + ", nodeType: " + elementImage.getNodeType());
                }

                phIndex++;
            }
        }

        update();

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
        root.add(populateScriptTree(imagePlaceHolders, imageLabels, imageActions, numberofVOIs));
        ((DefaultTreeModel) tree.getModel()).reload();
        expandAll(tree, true);

        // tree.addMouseListener(listener);
        tree.setDropTarget(new DropJTreeTarget());
        tree.setName("Script Tree");
        tree.setCellRenderer(new TreeRenderer());
        tree.addMouseListener(new TreeMouseAdapter());
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
     * Gets the list of images selected by the user in this dialog. Should be in the order that the images are used in
     * the script.
     *
     * @param  imageHolder  DOCUMENT ME!
     * @param  voiHolder    DOCUMENT ME!
     */
    @SuppressWarnings("unchecked")
    public void fillImagesVOIs(Vector<Vector<String>> imageHolder, Vector<Vector<String>> voiHolder) {

        boolean voiOnDisk = false;
        boolean imageOnDisk = false;
        boolean voiOnImage = false;

        ScriptTreeNode scriptNode = (ScriptTreeNode) root.getChildAt(0);

        int numImagePlaceHolders = scriptNode.getChildCount();
        int[] numImages = new int[numImagePlaceHolders];

        // find out how many "executers" we will have
        int numExecuters = ((ScriptTreeNode) scriptNode.getChildAt(0)).getChildCount();
        Vector<String>[] imageNames = new Vector[numExecuters];
        Vector<String>[] voiNames = new Vector[numExecuters];

        for (int i = 0; i < numExecuters; i++) {
            imageNames[i] = new Vector<String>();
            voiNames[i] = new Vector<String>();
        }


        ScriptTreeNode imagePHNode = null;
        ScriptTreeNode imageNode = null;
        int numVOIs;
        String voiName = null;
        ScriptVOI tempVOI = null;

        for (int i = 0; i < numImagePlaceHolders; i++) {
            imagePHNode = (ScriptTreeNode) scriptNode.getChildAt(i);
            numImages[i] = imagePHNode.getChildCount();

            for (int j = 0; j < numImages[i]; j++) {
                imageNode = (ScriptTreeNode) imagePHNode.getChildAt(j);
                imageNames[j].add(((String) imageNode.getUserObject()).trim());
                imageOnDisk = model.getScriptImage(((String) imageNode.getUserObject()).trim()).isOpenedByScript();

                numVOIs = imageNode.getChildCount();

                for (int k = 0; k < numVOIs; k++) {
                    voiName = (String) ((ScriptTreeNode) imageNode.getChildAt(k)).getUserObject();
                    tempVOI = model.getScriptImage(((String) imageNode.getUserObject()).trim()).getScriptVOI(voiName);

                    if (tempVOI.isOpenedByDialog()) {
                        voiOnDisk = true;
                        voiNames[j].add(tempVOI.getVoiFileLocation());
                    } else {
                        voiOnImage = true;
                        voiNames[j].add(voiName);
                    }
                    // voiNames[j].add(((String)((ScriptTreeNode)imageNode.getChildAt(k)).getUserObject()).trim());
                }

                // check to see if the image is NOT on disk, if it requires VOIs,
                // and if those VOIs are only on Disk
                // if it meets these requirements, check to see if there are currently
                // VOIs on the image, meaning these could interfere with the script
                // request to delete all (user can say yes/no)
                if (!imageOnDisk && (numVOIs > 0)) {

                    if (voiOnDisk && !voiOnImage) {
                        ModelImage tempImage = ViewUserInterface.getReference().getRegisteredImageByName(((String)
                                                                                                              imageNode.getUserObject())
                                                                                                             .trim());

                        if (tempImage != null) {

                            if (tempImage.getVOIs().size() > 0) {
                                int response = JOptionPane.showConfirmDialog(null,
                                                                             "Possible conflict with VOIs already loaded into image:  Delete existing VOIs?",
                                                                             "Warning", JOptionPane.YES_NO_OPTION);

                                if (response == JOptionPane.YES_OPTION) {
                                    tempImage.getVOIs().removeAllElements();
                                }
                            }
                        }
                    }
                }

            }
        }

        for (int i = 0; i < numExecuters; i++) {
            imageHolder.add(imageNames[i]);
            voiHolder.add(voiNames[i]);
        }

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
    public JList getImageList() {
        return this.imageList;
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
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public JList getVOIList() {
        return this.voiList;
    }
    
    
    /**
     * Returns the number of child nodes the script node has
     * @return
     */
    public int getScriptNodeChildCount() {
		return scriptNodeChildCount;
	}

	/**
     * DOCUMENT ME!
     */
    public void update() {
        int selectedIndex = imageList.getSelectedIndex();
        imageList.setListData(model.getAvailableImageList());

        if ((model.getAvailableImageList().size() > 0) && (selectedIndex == -1)) {
            selectedIndex = 0;
        }

        imageList.setSelectedIndex(selectedIndex);

        ScriptImage image = (ScriptImage) imageList.getSelectedValue();

        if (image == null) {
            voiList.setListData(emptyVector);

            return;
        }

        if (image.getScriptVOIs().length > 0) {
            voiList.setListData(image.getScriptVOIs());
            voiList.setSelectedIndex(0);
        } else {
            voiList.setListData(emptyVector);
            // voiList.setListData(new Vector());
        }

    }


    /**
     * Checks tree to see that all place holders have been replaced with actual images and VOIs if not, it prompts the
     * user with a warning message, and highlights the first node that still contains a placeholer.
     *
     * @return  <code>True</code> if all of the image and voi placeholders in all script executors have had images/vois
     *          assigned to them, <code>false</code> otherwise.
     */
    protected boolean isTreeReadyForScriptExecution() {
        ScriptTreeNode scriptNode = (ScriptTreeNode) root.getChildAt(0);


        int numImagePlaceHolders = scriptNode.getChildCount();
        scriptNodeChildCount = scriptNode.getChildCount();
        if(scriptNodeChildCount == 0) {
        	return true;
        }
        
        int[] numImages = new int[numImagePlaceHolders];

        ScriptTreeNode imagePHNode = null;
        ScriptTreeNode imageNode = null;
        int numVOIs;

        for (int i = 0; i < numImagePlaceHolders; i++) {
            imagePHNode = (ScriptTreeNode) scriptNode.getChildAt(i);
            numImages[i] = imagePHNode.getChildCount();

            for (int j = 0; j < numImages[i]; j++) {
                imageNode = (ScriptTreeNode) imagePHNode.getChildAt(j);
                numVOIs = imageNode.getChildCount();

                for (int k = 0; k < numVOIs; k++) {

                    if (((ScriptTreeNode) imageNode.getChildAt(k)).getUserObject().equals(VOI_EMPTY)) {
                        tree.setSelectionPath(new TreePath(((ScriptTreeNode) imageNode.getChildAt(k)).getPath()));
                        MipavUtil.displayWarning("VOI Placeholder is empty");

                        return false;
                    }
                }
            }
        }

        // now make sure we have equal numbers under each placeholder

        if (numImages[0] == 0) {
            tree.setSelectionPath(new TreePath(((ScriptTreeNode) scriptNode.getChildAt(0)).getPath()));
            MipavUtil.displayWarning("Placeholder is missing an image");

            return false;
        }

        for (int i = 0; i < (numImages.length - 1); i++) {

            if (numImages[i + 1] == 0) {
                tree.setSelectionPath(new TreePath(((ScriptTreeNode) scriptNode.getChildAt(i + 1)).getPath()));
                MipavUtil.displayWarning("Placeholder is missing an image");

                return false;
            } else if (numImages[i] != numImages[i + 1]) {
                TreePath[] paths = new TreePath[2];
                paths[0] = new TreePath(((ScriptTreeNode) scriptNode.getChildAt(i)).getPath());
                paths[1] = new TreePath(((ScriptTreeNode) scriptNode.getChildAt(i + 1)).getPath());
                tree.setSelectionPaths(paths);
                MipavUtil.displayWarning("Unequal number of images under each image placeholder");

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
     * @param  name  DOCUMENT ME!
     */
    private void addScrollList(String name) {
        Object[] contents = null;

        if (name.equalsIgnoreCase("Images List")) {
            contents = model.getAvailableImageList().toArray();
            
            
        } else if (name.equalsIgnoreCase("VOIs from above image List")) {

            if ((model.getAvailableImageList() != null) && (model.getAvailableImageList().size() > 0)) {
                contents = ((ScriptImage) model.getAvailableImageList().get(0)).getScriptVOIs();
            }

        }

        JList list = new JList(populateModel(contents));
        list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        list.setDragEnabled(true);
        list.setName(name);
        list.setSelectedIndex(0);
        list.setTransferHandler(new ArrayListTransferHandler());
        list.setCellRenderer(new CustomCellRenderer());
        list.addMouseListener(new TreeMouseAdapter());
        list.addMouseMotionListener(new TreeMouseDragAdapter());
        list.setDropTarget(new DropJListTarget());

        if (name.equalsIgnoreCase("Images List")) {
            imageList = list;
            imageList.addListSelectionListener(this);
        } else if (name.equalsIgnoreCase("VOIs from above image List")) {
            voiList = list;
        }


        JScrollPane scroll = new JScrollPane(list, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                             JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED) {
            public Dimension getPreferredSize() {
                changeSize();

                return scroll_size;
            }
        };
        scroll.setName(name + ": scroll");
        
        //set the popup menu
        if (name.equalsIgnoreCase("Images List")) {
        	
        }
        
        frame.getContentPane().add(scroll);
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
     * Creates the JFrame.
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

        String[] buttonNames = { "Run Script", "Add image from file", "Add VOI from file" };

        for (int c = 0; c < buttonNames.length; c++) {
            addButton(buttonNames[c]);
        }

        String[] fileMenuItems = {
            "File", "Open saved image and VOI selections", "Save current image and VOI selections",
            "View current script contents", "Close"
        };

        JMenuBar menuBar = new JMenuBar();
        JMenu menu = new JMenu(fileMenuItems[0]);
        menuBar.add(menu);

        for (int i = 1; i < fileMenuItems.length; i++) {
            JMenuItem item = new JMenuItem(fileMenuItems[i]);
            item.setActionCommand(fileMenuItems[i]);
            item.addActionListener(this);
            menu.add(item);
        }

        String[] helpMenuItems = { "Help", "Scripting help" };

        menu = new JMenu(helpMenuItems[0]);
        menuBar.add(menu);

        for (int i = 1; i < helpMenuItems.length; i++) {
            JMenuItem item = new JMenuItem(helpMenuItems[i]);
            item.setActionCommand(helpMenuItems[i]);
            item.addActionListener(this);
            menu.add(item);
        }

        frame.setJMenuBar(menuBar);

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
    @SuppressWarnings("unchecked")
    private void expandAll(JTree tree, TreePath parent, boolean expand) {
        TreeNode node = (TreeNode) parent.getLastPathComponent();

        if (node.getChildCount() >= 0) {

            for (Enumeration<TreeNode> e = node.children(); e.hasMoreElements();) {
                TreeNode n = e.nextElement();
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
     * DOCUMENT ME!
     *
     * @param   imagePlaceHolders  DOCUMENT ME!
     * @param   imageLabels        DOCUMENT ME!
     * @param   imageActions       DOCUMENT ME!
     * @param   numberofVOIs       DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private ScriptTreeNode populateScriptTree(String[] imagePlaceHolders, String[] imageLabels, String[] imageActions,
                                              int[] numberofVOIs) {
        ScriptTreeNode newNode = new ScriptTreeNode("Script Executer", JDialogRunScriptView.SCRIPTNODE);

        ScriptTreeNode[] imagePlaceHolderNodes;
        imagePlaceHolderNodes = new ScriptTreeNode[imagePlaceHolders.length];

        for (int i = 0; i < imagePlaceHolders.length; i++) {
            imagePlaceHolderNodes[i] = new ScriptTreeNode(imagePlaceHolders[i] + " (" + imageActions[i] + " -- " +
                                                          imageLabels[i] + ")", JDialogRunScriptView.IMAGEPLACEHOLDERNODE);
            newNode.add(imagePlaceHolderNodes[i]);
        }

        return newNode;
    }

    /**
     * Called when a script is loaded from disk.
     *
     * @param   executer  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private ScriptTreeNode restoreSavedExecuter(Node executer) {
        //System.out.println("executer: " + executer.getNodeName());

        ScriptTreeNode newNode = new ScriptTreeNode("Script Executer", JDialogRunScriptView.SCRIPTNODE);
        ScriptTreeNode[] imageNodes = new ScriptTreeNode[executer.getChildNodes().getLength()];
        ScriptTreeNode voi = null;
        //System.out.println("imageNodes length: " + imageNodes.length);
        //System.out.println("first Child: " + executer.getFirstChild().getNodeName());

        // loop over images
        for (int i = 1; i < executer.getChildNodes().getLength(); i += 2) {
            int ii = 0;
            imageNodes[ii] = new ScriptTreeNode(executer.getChildNodes().item(i).getAttributes().getNamedItem("name").getNodeValue(),
                                                JDialogRunScriptView.IMAGENODE);

            imageNodes[ii].setFilePath(executer.getChildNodes().item(i).getAttributes().getNamedItem("filePath").getNodeValue());

            // imageNodes[ii] = new
            // ScriptTreeNode(executer.getChildNodes().item(i).getNodeName().replace("__",
            // " "), this.IMAGENODE);
            newNode.add(imageNodes[ii]);
            //System.out.println("newNode = " + imageNodes[ii]);

            for (int j = 1; j < executer.getChildNodes().item(i).getChildNodes().getLength(); j += 2) {
                voi = new ScriptTreeNode(executer.getChildNodes().item(i).getChildNodes().item(j).getNodeName().replaceAll("__",
                                                                                                                           " "),
                                         JDialogRunScriptView.VOINODE);
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

        ((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, parentNode, parentNode.getIndex(node));
        ((DefaultTreeModel) tree.getModel()).removeNodeFromParent(node);
        expandAll(tree, new TreePath(newNode.getPath()), true);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Transfer handler used for dragging the images and vois into the tree.
     */
    private class ArrayListTransferHandler extends TransferHandler {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 1339840969054696153L;

        /** DOCUMENT ME! */
        DataFlavor localArrayListFlavor, serialArrayListFlavor;

        /** DOCUMENT ME! */
        String localArrayListType = DataFlavor.javaJVMLocalObjectMimeType + ";class=java.util.ArrayList";

        /** DOCUMENT ME! */
        JList source = null;

        /**
         * Creates a new ArrayListTransferHandler object.
         */
        public ArrayListTransferHandler() {

            try {
                localArrayListFlavor = new DataFlavor(localArrayListType);
            } catch (ClassNotFoundException e) {
                System.out.println("ArrayListTransferHandler: unable to create data flavor");
            }

            serialArrayListFlavor = new DataFlavor(ArrayList.class, "ArrayList");
        }

        /**
         * DOCUMENT ME!
         *
         * @param   c        DOCUMENT ME!
         * @param   flavors  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public boolean canImport(JComponent c, DataFlavor[] flavors) {

            if (hasLocalArrayListFlavor(flavors)) {
                return true;
            }

            if (hasSerialArrayListFlavor(flavors)) {
                return true;
            }

            return false;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   c  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getSourceActions(JComponent c) {
            return COPY_OR_MOVE;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   c  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        protected Transferable createTransferable(JComponent c) {

            if (c instanceof JList) {
                source = (JList) c;

                Object[] values = source.getSelectedValues();

                if ((values == null) || (values.length == 0)) {
                    return null;
                }

                ArrayList<String> alist = new ArrayList<String>(values.length);

                for (int i = 0; i < values.length; i++) {
                    Object o = values[i];
                    String str = o.toString();

                    if (str == null) {
                        str = "";
                    }

                    alist.add(str);
                }

                return new ArrayListTransferable(alist);
            }

            return null;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   flavors  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        private boolean hasLocalArrayListFlavor(DataFlavor[] flavors) {

            if (localArrayListFlavor == null) {
                return false;
            }

            for (int i = 0; i < flavors.length; i++) {

                if (flavors[i].equals(localArrayListFlavor)) {
                    return true;
                }
            }

            return false;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   flavors  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        private boolean hasSerialArrayListFlavor(DataFlavor[] flavors) {

            if (serialArrayListFlavor == null) {
                return false;
            }

            for (int i = 0; i < flavors.length; i++) {

                if (flavors[i].equals(serialArrayListFlavor)) {
                    return true;
                }
            }

            return false;
        }

        /**
         * DOCUMENT ME!
         */
        public class ArrayListTransferable implements Transferable {

            /** DOCUMENT ME! */
            ArrayList<String> data;

            /**
             * Creates a new ArrayListTransferable object.
             *
             * @param  alist  DOCUMENT ME!
             */
            public ArrayListTransferable(ArrayList<String> alist) {
                data = alist;
            }

            /**
             * DOCUMENT ME!
             *
             * @param   flavor  DOCUMENT ME!
             *
             * @return  DOCUMENT ME!
             *
             * @throws  UnsupportedFlavorException  DOCUMENT ME!
             */
            public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException {

                if (!isDataFlavorSupported(flavor)) {
                    throw new UnsupportedFlavorException(flavor);
                }

                return data;
            }

            /**
             * DOCUMENT ME!
             *
             * @return  DOCUMENT ME!
             */
            public DataFlavor[] getTransferDataFlavors() {
                return new DataFlavor[] { localArrayListFlavor, serialArrayListFlavor };
            }

            /**
             * DOCUMENT ME!
             *
             * @param   flavor  DOCUMENT ME!
             *
             * @return  DOCUMENT ME!
             */
            public boolean isDataFlavorSupported(DataFlavor flavor) {

                if (localArrayListFlavor.equals(flavor)) {
                    return true;
                }

                if (serialArrayListFlavor.equals(flavor)) {
                    return true;
                }

                return false;
            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    private class CustomCellRenderer extends JLabel implements ListCellRenderer {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -5674415094245231848L;

        /**
         * Creates a new CustomCellRenderer object.
         */
        public CustomCellRenderer() {

            // Don't paint behind the component
            setOpaque(true);
        }

        /**
         * Set the attributes of the class and return a reference.
         *
         * @param   list   DOCUMENT ME!
         * @param   value  DOCUMENT ME!
         * @param   index  DOCUMENT ME!
         * @param   iss    DOCUMENT ME!
         * @param   chf    DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Component getListCellRendererComponent(JList list, Object value, // value to display
                                                      int index, // cell index
                                                      boolean iss, // is selected
                                                      boolean chf) // cell has focus?
        {

            // Set the text and
            // background color for rendering

            if (value instanceof ScriptImage) {

                if (((ScriptImage) value).isOpenedByScript()) {

                    if (((ScriptImage) value).isMultiFile()) {
                        setToolTipText("[multi-file]" + ((ScriptImage) value).getFileLocation());
                    } else {
                        setToolTipText(((ScriptImage) value).getFileLocation());
                    }

                    setForeground(Color.BLUE);
                } else {
                    setToolTipText(((ScriptImage) value).getFileLocation());
                    setForeground(Color.BLACK);
                }

                setText(((ScriptImage) value).getImageName());


            } else if (value instanceof ScriptVOI) {

                if (((ScriptVOI) value).isOpenedByDialog() == false) {
                    setForeground(Color.BLACK);
                } else {
                    setForeground(Color.BLUE);
                    setToolTipText(((ScriptVOI) value).getVoiFileLocation());
                }

                setText(((ScriptVOI) value).toString());

            }


            // Set a border if the
            // list item is selected
            if (iss) {
                setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
                setBackground(Color.LIGHT_GRAY);
            } else {
                setBorder(null);
                setBackground(list.getBackground());
            }

            return this;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private class DropJListTarget extends DropTarget {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 8720669794812124831L;

        /**
         * DOCUMENT ME!
         *
         * @param  dtde  DOCUMENT ME!
         */
        public void drop(DropTargetDropEvent dtde) {

            Point pt = dtde.getLocation();

            if (voiList.contains(pt)) {
          //      System.err.println("VOI");
            } else if (imageList.contains(pt)) {
            //    System.err.println("IMAGE");
            }


            if ((dtde.getSource().equals(imageList) && (dropSource == VOI_DROP)) ||
                    (dtde.getSource().equals(voiList) && (dropSource == IMAGE_DROP))) {
           //     System.err.println("different source and target");
            } else {
            //    System.err.println("GOT A DROP!");
            }


        }
    }

    /**
     * Class used to handle the dragging/dropping of images/vois into the script tree.
     *
     * @author  linkb
     */
    private class DropJTreeTarget extends DropTarget {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -3115134852321228978L;

        /**
         * DOCUMENT ME!
         *
         * @param  dtde  DOCUMENT ME!
         */
        @SuppressWarnings("unchecked")
        public void drop(DropTargetDropEvent dtde) {
            Transferable transferable = dtde.getTransferable();

            // DataFlavor[] flavors = transferable.getTransferDataFlavors();
            DataFlavor dataFlavor = null;

            try {
                dataFlavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=java.util.ArrayList");
            } catch (ClassNotFoundException c) {
                c.printStackTrace();
            }

            String[] text = null;
            ArrayList<String> list = null;

            try {
                list = ((ArrayList<String>) transferable.getTransferData(dataFlavor));
                text = new String[list.size()];

                for (int i = 0; i < text.length; i++) {
                    text[i] = list.get(i);
                }
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

            ScriptTreeNode targetNode = (ScriptTreeNode) pathTarget.getLastPathComponent();
            ScriptTreeNode newNode = null;
            ScriptTreeNode parentNode = (ScriptTreeNode) targetNode.getParent();

            int targetNodeType = targetNode.getNodeType();
            int newNodeType = IMAGENODE;

            if ((targetNodeType == SCRIPTNODE) && (dropSource == VOI_DROP)) {
                return;
            }


            // set up what type of node the new node will be
            if ((targetNodeType == IMAGEPLACEHOLDERNODE) || (targetNodeType == IMAGENODE) ||
                    (targetNodeType == SCRIPTNODE)) {
                newNodeType = IMAGENODE;
            } else if ((targetNodeType == VOINODE) || (targetNodeType == VOINODE)) {
                newNodeType = VOINODE;
            }

            ScriptTreeNode voiNode = null;

            int voiCount = 0;
            int childCount = 0;
            int currentVOIIndex = 0;
            int placeholderIndex = 0;

            childCount = targetNode.getChildCount();

            int targetIndex = targetNode.getParent().getIndex(targetNode);
            int targetParentIndex = 0;

            // add or replace nodes one by one
            for (int i = 0; i < text.length; i++) {

                if ((targetNodeType == SCRIPTNODE) && (dropSource == IMAGE_DROP)) {

                    // going to add to as many placeholders as possible (depends on number of images dropped)
                    if (placeholderIndex < childCount) {
                        newNode = new ScriptTreeNode(text[i], newNodeType);
                        voiCount = model.getNumberOfRequiredVOIsForScriptImages()[placeholderIndex];

                        for (int j = 0; j < voiCount; j++) {
                            voiNode = new ScriptTreeNode(VOI_EMPTY, VOINODE);
                            newNode.add(voiNode);
                        }

                        int numVOI = model.getScriptImage(text[i]).getScriptVOIs().length;

                        for (int j = 0; (j < numVOI) && (j < voiCount); j++) {
                            updateNode((ScriptTreeNode) newNode.getChildAt(j),
                                       model.getScriptImage(text[i]).getScriptVOIs()[j].getVoiName());
                        }

                        ((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode,
                                                                            (ScriptTreeNode)
                                                                            targetNode.getChildAt(placeholderIndex),
                                                                            ((ScriptTreeNode)
                                                                                 targetNode.getChildAt(placeholderIndex))
                                                                                .getChildCount());
                        placeholderIndex++;

                    } else {
                        return;
                    }

                } else if ((targetNodeType == IMAGEPLACEHOLDERNODE) && (dropSource == IMAGE_DROP)) {
                    newNode = new ScriptTreeNode(text[i], newNodeType);
                    // inserting into placeholder as new image node look for last image child and append after that

                    // first add all required VOIs
                    voiCount = model.getNumberOfRequiredVOIsForScriptImages()[targetIndex];


                    for (int j = 0; j < voiCount; j++) {
                        voiNode = new ScriptTreeNode(VOI_EMPTY, VOINODE);
                        newNode.add(voiNode);
                    }

                    int numVOI = model.getScriptImage(text[i]).getScriptVOIs().length;

                    for (int j = 0; (j < numVOI) && (j < voiCount); j++) {
                        updateNode((ScriptTreeNode) newNode.getChildAt(j),
                                   model.getScriptImage(text[i]).getScriptVOIs()[j].getVoiName());
                    }

                    ((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, targetNode, childCount);

                    // expandAll(tree, new TreePath(newNode.getPath()), true);
                } else if ((targetNodeType == IMAGENODE) && (dropSource == IMAGE_DROP)) {

                    // System.err.println("TARGET TYPE IMAGENODE, SOURCE TYPE IMAGE");
                    newNode = new ScriptTreeNode(text[i], newNodeType);
                    targetNode.removeAllChildren();

                    targetParentIndex = targetNode.getParent().getParent().getIndex(targetNode.getParent());

                    voiCount = model.getNumberOfRequiredVOIsForScriptImages()[targetParentIndex];

                    for (int j = 0; j < voiCount; j++) {
                        voiNode = new ScriptTreeNode(VOI_EMPTY, VOINODE);
                        newNode.add(voiNode);
                    }

                    ((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, parentNode,
                                                                        parentNode.getIndex(targetNode));
                    ((DefaultTreeModel) tree.getModel()).removeNodeFromParent(targetNode);
                    expandAll(tree, true);

                    return;

                } else if ((targetNodeType == VOINODE) && (dropSource == VOI_DROP)) {

                    // System.err.println("TARGET TYPE VOINODE, SOURCE TYPE VOI");
                    String targetImageName = (String) ((ScriptTreeNode) targetNode.getParent()).getUserObject();
                    String sourceImageName = ((JList)
                                                  ((JScrollPane) getComponentByName("Images List: scroll"))
                                                      .getViewport().getView()).getSelectedValue().toString();

                    if (!(targetImageName.equalsIgnoreCase(sourceImageName))) {
                        MipavUtil.displayWarning("Source image does not contain this VOI");
                        dtde.acceptDrop(action);
                        dtde.dropComplete(true);

                        return;
                    } else {
                        updateNode(targetNode, text[i]);

                        return;
                    }

                } else if ((targetNodeType == IMAGENODE) && (dropSource == VOI_DROP)) {

                    // System.err.println("TARGET TYPE IMAGENODE, SOURCE TYPE IMAGE");
                    String targetImageName = (String) targetNode.getUserObject();
                    String sourceImageName = ((JList)
                                                  ((JScrollPane) getComponentByName("Images List: scroll"))
                                                      .getViewport().getView()).getSelectedValue().toString();

                    if (!(targetImageName.equalsIgnoreCase(sourceImageName))) {
                        MipavUtil.displayWarning("Source image does not contain this VOI");

                        return;
                    }

                    // populate as many VOI nodes as have been dropped...if conditions allow
                    if (childCount > currentVOIIndex) {

                        // System.err.println("Current VOI Index is: " + currentVOIIndex);
                        voiNode = (ScriptTreeNode) targetNode.getChildAt(currentVOIIndex);
                        updateNode(voiNode, text[i]);
                        currentVOIIndex++;
                    }

                }
            }

            dtde.acceptDrop(action);
            dtde.dropComplete(true);

            expandAll(tree, true);
        }
    }

    /**
     * DOCUMENT ME!
     */
    private class TreeMouseAdapter extends MouseAdapter {

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

            if (((Component) e.getSource()).getName().equalsIgnoreCase("Script Tree")) {

                if (e.getModifiers() == InputEvent.BUTTON3_MASK) {
                    int type;
                    Point pt = e.getPoint();
                    TreePath pathTarget = tree.getPathForLocation(pt.x, pt.y);
                    selectedNode = (ScriptTreeNode) pathTarget.getLastPathComponent();
                    selectedNodeName = (String) selectedNode.getUserObject();

                    type = selectedNode.getNodeType();

                    if ((type == IMAGEPLACEHOLDERNODE) || (type == SCRIPTNODE) ||
                            ((type == VOINODE) && selectedNodeName.equals(VOI_EMPTY))) {
                        return;
                    } else {
                        JMenuItem deleteMenuItem = new JMenuItem("Delete");
                        deleteMenuItem.addActionListener(new java.awt.event.ActionListener() {
                                public void actionPerformed(java.awt.event.ActionEvent e) {

                                    if (selectedNode.getNodeType() == VOINODE) {
                                        updateNode(selectedNode, VOI_EMPTY);
                                    } else {
                                        DefaultTreeModel model = (DefaultTreeModel) tree.getModel();
                                        model.removeNodeFromParent(selectedNode);
                                    }

                                    expandAll(tree, true);
                                }
                            } // new ActionListener
                                                        ); // addActionListener

                        JPopupMenu popup = new JPopupMenu();
                        popup.add(deleteMenuItem);

                        // popup.add(applyAllMenuItem);
                        popup.show(tree, pt.x + 10, pt.y + 10);
                    }

                } // right mouse button clicked

            } // source equals scriptTree

            if (e.getSource().equals(imageList)) {
                dropSource = IMAGE_DROP;

                selectedListIndicies = imageList.getSelectedIndices();

                // turn off the VOI Drag option as long as more than 1 image is selected
                if (selectedListIndicies.length > 1) {
                    voiList.setListData(emptyVector);
                } else {
                    ScriptImage image = (ScriptImage) ((JList) e.getSource()).getSelectedValue();

                    if (image == null) {
                        return;
                    }

                    if (image.getScriptVOIs().length > 0) {
                        voiList.setListData(image.getScriptVOIs());
                        voiList.setSelectedIndex(0);
                    } else {
                        voiList.setListData(emptyVector);
                        // voiList.setListData(new Vector());
                    }
                }

            } else if (e.getSource().equals(voiList)) {
                dropSource = VOI_DROP;
            }
        }
    }


    /**
     * DOCUMENT ME!
     */
    private class TreeMouseDragAdapter extends MouseMotionAdapter {

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseDragged(MouseEvent e) {

            if (e.getSource().equals(imageList)) {
                TransferHandler th = imageList.getTransferHandler();
                th.exportAsDrag(imageList, e, TransferHandler.COPY);
            } else if (e.getSource().equals(voiList)) {
                TransferHandler th = voiList.getTransferHandler();
                th.exportAsDrag(voiList, e, TransferHandler.COPY);
            }
        }
    }

    /**
     * ***************************************************************************************************** Inner
     * classes to follow************************************************************************************************
     * ***** protected class ModelImageForScripting{ ModelImage modelImage; ModelImageForScripting(ModelImage
     * modelImage){ this.modelImage = modelImage; } public ModelImage getModelImage(){ return this.modelImage; } public
     * String toString(){ return modelImage.getImageName(); } }.
     */
    private class TreeRenderer extends DefaultTreeCellRenderer {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 5484058426833940837L;

        /** DOCUMENT ME! */
        Icon imageIcon, voiIcon, scriptIcon, emptyIcon;

        /**
         * Creates a new TreeRenderer object.
         */
        public TreeRenderer() {
            this.imageIcon = MipavUtil.getIcon("cube.gif");
            this.voiIcon = MipavUtil.getIcon("polygon.gif");
            this.scriptIcon = MipavUtil.getIcon("script.gif");
            this.emptyIcon = MipavUtil.getIcon("emptytree.gif");
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

            if ((node.getNodeType() == IMAGEPLACEHOLDERNODE) && (node.getChildCount() > 0)) {
                this.setForeground(Color.BLACK);
            }

            if (node.getNodeType() == SCRIPTNODE) {
                setIcon(scriptIcon);
                this.setForeground(Color.BLACK);
            } else if (node.getNodeType() == IMAGEPLACEHOLDERNODE) {

                if (node.getChildCount() > 0) {
                    setForeground(Color.BLACK);
                } else {
                    setForeground(Color.GRAY);
                }

                setIcon(emptyIcon);
            } else if (node.getNodeType() == IMAGENODE) {
                setIcon(imageIcon);
                setForeground(Color.BLACK);

                int index = node.getParent().getIndex(node);
                setText("(run-" + (index + 1) + ") " + getText());

            } else if (node.getNodeType() == VOINODE) {

                if (((String) node.getUserObject()).equals(VOI_EMPTY)) {
                    setForeground(Color.GRAY);
                } else {
                    setForeground(Color.BLACK);
                }

                setIcon(voiIcon);
            } else {
                setIcon(null);
            }

            return this;
        }
    }
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
                popup.show(e.getComponent(), e.getX(), e.getY());
            }
        }
    }
    
} // class
