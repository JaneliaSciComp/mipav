package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import org.w3c.dom.*;

import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.awt.event.*;

import java.util.*;

import java.io.*;

import javax.swing.*;
import javax.swing.tree.*;


/**
 * @author   Nathan Pollack -- Contractor (SSAI)
 * @version  0.1 May 24, 2006
 * @see      JDialogRunScriptController
 * @see      JDialogRunScriptModel
 */
public class JDialogRunScriptView implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

	private static final String PLACEHOLDER_STR = "Image_Placeholder";
	private static final String IMAGE_STR = "Image";
	
    /** Script node type (Script)... only one and is only child of root */
    private static final int SCRIPTNODE = 0;

    /** Image placeholder node type never created nor deleted by user */
    private static final int IMAGEPLACEHOLDERNODE = 1;
    
    /** Image node type, falls under an imageplaceholder node,
     *  created and deleted by user */
    private static final int IMAGENODE = 2;
    
    /** VOI node type, begins with VOI_EMPTY string and must be
     * set if present before running of the script */
    private static final int VOINODE = 3;

    /** Root node type */
    private static final int ROOTNODE = 4;

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

    /** Keep a reference to the VOI List to turn on and off if multiple images are selected */
    private JList voiList = null;
        
    private JList imageList = null;
    
    private int dropSource = 0;

    private static final int IMAGE_DROP = 0;
    private static final int VOI_DROP   = 1;
    private static final int TREE_DROP  = 2;
    
    private static final String VOI_EMPTY = "[Insert VOI]";
    
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
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    public void update() {
    	imageList.setListData(model.getAvailableImageList());
    	imageList.setSelectedIndex(-1);
    }
    
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
     * @param  savedScript  DOCUMENT ME!
     */
    public void createScriptTree(org.w3c.dom.Document savedScript) {
        this.numberOfExecuters = 0;

        ScriptTreeNode scriptNode = (ScriptTreeNode)root.getChildAt(0);

    	
    	int numImagePlaceHolders = scriptNode.getChildCount();
    	
    	ScriptTreeNode imagePHNode = null;
    	int numImages;
    	DefaultTreeModel tModel = (DefaultTreeModel)tree.getModel();
		
    	//clean up old tree
    	for (int i = 0; i < numImagePlaceHolders; i++) {
    		imagePHNode = (ScriptTreeNode)scriptNode.getChildAt(i);
    		numImages = imagePHNode.getChildCount();
    		for (int j = numImages - 1; j >= 0; j--) {
    			tModel.removeNodeFromParent((ScriptTreeNode)imagePHNode.getChildAt(j));
    		}
    	}
       
    	int numDocPlaceHolders = savedScript.getDocumentElement().getChildNodes().getLength();
    	Node elementPH;
    	Node elementImage;
    	int numDocImages;
    	
    	String imageName;
    	String imageFilePath;
    	int phIndex = 0;
    	int voiCount = 0;
    	ScriptTreeNode newNode;
    	ScriptTreeNode targetNode;
    	ScriptTreeNode voiNode;
    	int childCount;
    	boolean isMulti;
    	NodeList children = savedScript.getDocumentElement().getChildNodes();
    	
    	//run through once and see if
    	int phCount = 0;
    	for (int i = 0; i < numDocPlaceHolders; i++) {
    		elementPH = (Node)children.item(i);
    		if (elementPH.getNodeType() == Node.ELEMENT_NODE &&
    				elementPH.getNodeName().equals(PLACEHOLDER_STR)) {
    			phCount++;
    		}
    	}
    	    
    	if (phCount != numImagePlaceHolders) {
    		MipavUtil.displayError("Script's image placeholders do not match saved image placeholders");
    		return;
    	}
    	
    	
    	
    	
    	for ( int i = 0; i < numDocPlaceHolders; i++) {
    		    		
    		elementPH = (Node)children.item(i);
    		if (elementPH.getNodeType() == Node.ELEMENT_NODE &&
    				elementPH.getNodeName().equals(PLACEHOLDER_STR)) {
    			numDocImages = elementPH.getChildNodes().getLength();
        		for (int j = 0; j < numDocImages; j++) {
        			elementImage = (Node)elementPH.getChildNodes().item(j);
        			if (elementImage.getNodeType() == Node.ELEMENT_NODE &&
        					elementImage.getNodeName().equals(IMAGE_STR)) {
        				imageName = elementImage.getAttributes().getNamedItem("name").getNodeValue();
        				imageFilePath = elementImage.getAttributes().getNamedItem("filePath").getNodeValue();
        				isMulti = Boolean.parseBoolean(elementImage.getAttributes().getNamedItem("isMulti").getNodeValue());
        				
        				targetNode = (ScriptTreeNode)scriptNode.getChildAt(phIndex);
        				childCount = targetNode.getChildCount();
        				newNode = new ScriptTreeNode(imageName, IMAGENODE);
        				newNode.setFilePath(imageFilePath);
                		//inserting into placeholder as new image node
                		// look for last image child and append after that
                		
                		//first add all required VOIs
                		voiCount = model.getNumberOfRequiredVOIsForScriptImages()[phIndex];
                		for (int k = 0; j < voiCount; k++) {
                			voiNode = new ScriptTreeNode(VOI_EMPTY, VOINODE);
                			newNode.add(voiNode);
                		}
                		
                		((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, targetNode, childCount);
        				
                		//check to see if the image is currently open, if not, add to list of Images on right
                		if (model.getScriptImage(imageName) == null) {
                			//not sure about the isMulti setting to true always...
                			model.addToAvailableImageList(imageName, imageFilePath, isMulti);
                		}
                		
        			}
        			
        			
        			//System.err.println("\t" + elementImage.getNodeName() + ", value: " + elementImage.getNodeValue() + ", nodeType: " + elementImage.getNodeType());
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
        root.add(createNewExecuter(imagePlaceHolders, imageLabels, imageActions, numberofVOIs));
        ((DefaultTreeModel) tree.getModel()).reload();
        expandAll(tree, true);
        tree.addMouseListener(listener);
        tree.setDropTarget(new DropJTreeLister());
        tree.setName("Script Tree");
        tree.setCellRenderer(new TreeRenderer());
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
    public void fillImagesVOIs(Vector imageHolder, Vector voiHolder) {
     
        ScriptTreeNode scriptNode = (ScriptTreeNode)root.getChildAt(0);
    	
    	int numImagePlaceHolders = scriptNode.getChildCount();
    	int [] numImages = new int[numImagePlaceHolders];
    	
    	//find out how many "executers" we will have
    	int numExecuters = ((ScriptTreeNode)scriptNode.getChildAt(0)).getChildCount();
    	Vector [] imageNames = new Vector[numExecuters];
    	Vector [] voiNames = new Vector[numExecuters];
    	
    	for (int i = 0; i < numExecuters; i++) {
    		imageNames[i] = new Vector();
    		voiNames[i] = new Vector();
    	}
    	
    	
    	ScriptTreeNode imagePHNode = null;
    	ScriptTreeNode imageNode = null;
    	int numVOIs;
    	
    	for (int i = 0; i < numImagePlaceHolders; i++) {
    		imagePHNode = (ScriptTreeNode)scriptNode.getChildAt(i);
    		numImages[i] = imagePHNode.getChildCount();
    		for (int j = 0; j < numImages[i]; j++) {
    			imageNode = (ScriptTreeNode)imagePHNode.getChildAt(j);
    			imageNames[j].add(((String)imageNode.getUserObject()).trim());
    			
    			
    			numVOIs = imageNode.getChildCount();
    			for (int k = 0; k < numVOIs; k++) {
    				voiNames[j].add(((String)((ScriptTreeNode)imageNode.getChildAt(k)).getUserObject()).trim());
    			}
    		}
    	}
    	
    	for (int i = 0; i < numExecuters; i++) {
    		imageHolder.add(imageNames[i]);
    		voiHolder.add(voiNames[i]);
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
    	ScriptTreeNode scriptNode = (ScriptTreeNode)root.getChildAt(0);

	
    	int numImagePlaceHolders = scriptNode.getChildCount();
    	int [] numImages = new int[numImagePlaceHolders];
    	
    	ScriptTreeNode imagePHNode = null;
    	ScriptTreeNode imageNode = null;
    	int numVOIs;
    	
    	for (int i = 0; i < numImagePlaceHolders; i++) {
    		imagePHNode = (ScriptTreeNode)scriptNode.getChildAt(i);
    		numImages[i] = imagePHNode.getChildCount();
    		for (int j = 0; j < numImages[i]; j++) {
    			imageNode = (ScriptTreeNode)imagePHNode.getChildAt(j);
    			numVOIs = imageNode.getChildCount();
    			for (int k = 0; k < numVOIs; k++) {
    				if ( ((ScriptTreeNode)imageNode.getChildAt(k)).getUserObject().equals(VOI_EMPTY)  ) {
    					tree.setSelectionPath(new TreePath(((ScriptTreeNode)imageNode.getChildAt(k)).getPath()));
    					MipavUtil.displayWarning("VOI Placeholder is empty");
    					
    					return false;
    				}
    			}
    		}
    	}
    	
    	//now make sure we have equal numbers under each placeholder
    	
    	if (numImages[0] == 0) {
    		tree.setSelectionPath(new TreePath(((ScriptTreeNode)scriptNode.getChildAt(0)).getPath()));
			MipavUtil.displayWarning("Placeholder is missing an image");
			return false;
    	}
    	
    	for (int i = 0; i < numImages.length -1 ; i++) {
    		
    		if (numImages[i+1] == 0) {
    			tree.setSelectionPath(new TreePath(((ScriptTreeNode)scriptNode.getChildAt(i+1)).getPath()));
    			MipavUtil.displayWarning("Placeholder is missing an image");
    			return false;
    		} else if (numImages[i] != numImages[i+1]) {
    			TreePath [] paths = new TreePath[2];
    			paths[0] = new TreePath(((ScriptTreeNode)scriptNode.getChildAt(i)).getPath());
    			paths[1] = new TreePath(((ScriptTreeNode)scriptNode.getChildAt(i+1)).getPath());
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
        list.addMouseListener(listener);
        list.setName(name);
        list.setSelectedIndex(0);
        list.setTransferHandler(new ArrayListTransferHandler());
        list.setCellRenderer(new CustomCellRenderer());
        
        
        if (name.equalsIgnoreCase("Images List")) {
            imageList = list;
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

            if (selectedNode.getNodeType() == IMAGENODE) {
                copyNodes[j] = node;
            } else if (selectedNode.getNodeType() == VOINODE) {
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

        ScriptTreeNode[] imagePlaceHolderNodes;
        ScriptTreeNode voi = null;
        imagePlaceHolderNodes = new ScriptTreeNode[imagePlaceHolders.length];

        for (int i = 0; i < imagePlaceHolders.length; i++) {
        	imagePlaceHolderNodes[i] = new ScriptTreeNode(imagePlaceHolders[i] + " (" + imageActions[i] + " -- " + imageLabels[i] +
                                               ")", this.IMAGEPLACEHOLDERNODE);
            newNode.add(imagePlaceHolderNodes[i]);

      //      for (int j = 0; j < numberofVOIs[i]; j++) {
       //         voi = new ScriptTreeNode("VOI", this.VOIPLACEHOLDERNODE);
       //         imagePlaceHolderNodes[i].add(voi);
         //   }
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

        String[] buttonNames = { "Run Script", "Add image from file", "Add VOI from file" };

        for (int c = 0; c < buttonNames.length; c++) {
            addButton(buttonNames[c]);
        }

        //String[] menuItems = {
        //    "File", "View current script contents", "Close"
        //};
        
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

        ((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, parentNode, parentNode.getIndex(node));
        ((DefaultTreeModel) tree.getModel()).removeNodeFromParent(node);
        expandAll(tree, new TreePath(newNode.getPath()), true);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    
    private class ArrayListTransferHandler extends TransferHandler {
    	DataFlavor localArrayListFlavor, serialArrayListFlavor;
        String localArrayListType = DataFlavor.javaJVMLocalObjectMimeType +
                                    ";class=java.util.ArrayList";
        JList source = null;
        int[] indices = null;
        int addIndex = -1; //Location where items were added
        int addCount = 0;  //Number of items added

        public ArrayListTransferHandler() {
            try {
                localArrayListFlavor = new DataFlavor(localArrayListType);
            } catch (ClassNotFoundException e) {
                System.out.println(
                 "ArrayListTransferHandler: unable to create data flavor");
            }
            serialArrayListFlavor = new DataFlavor(ArrayList.class,
                                                  "ArrayList");
        }

       

       

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

        public boolean canImport(JComponent c, DataFlavor[] flavors) {
            if (hasLocalArrayListFlavor(flavors))  { return true; }
            if (hasSerialArrayListFlavor(flavors)) { return true; }
            return false;
        }

        protected Transferable createTransferable(JComponent c) {
            if (c instanceof JList) {
                source = (JList)c;
                indices = source.getSelectedIndices();
                Object[] values = source.getSelectedValues();
                if (values == null || values.length == 0) {
                    return null;
                }
                ArrayList alist = new ArrayList(values.length);
                for (int i = 0; i < values.length; i++) {
                    Object o = values[i];
                    String str = o.toString();
                    if (str == null) str = "";
                    alist.add(str);
                }
                return new ArrayListTransferable(alist);
            }
            return null;
        }

        public int getSourceActions(JComponent c) {
            return COPY_OR_MOVE;
        }

        public class ArrayListTransferable implements Transferable {
            ArrayList data;

            public ArrayListTransferable(ArrayList alist) {
                data = alist;
            }

            public Object getTransferData(DataFlavor flavor)
                                     throws UnsupportedFlavorException {
                if (!isDataFlavorSupported(flavor)) {
                    throw new UnsupportedFlavorException(flavor);
                }
                return data;
            }

            public DataFlavor[] getTransferDataFlavors() {
                return new DataFlavor[] { localArrayListFlavor,
                                          serialArrayListFlavor };
            }

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
//System.err.println("MOUSE PRESSED");
            if (((Component) e.getSource()).getName().equalsIgnoreCase("Script Tree")) {

                if (e.getModifiers() == InputEvent.BUTTON3_MASK) {
                	int type;
                    Point pt = e.getPoint();
                    TreePath pathTarget = tree.getPathForLocation(pt.x, pt.y);
                    selectedNode = (ScriptTreeNode) pathTarget.getLastPathComponent();
                    parentNode = (ScriptTreeNode) selectedNode.getParent();
                    selectedNodeName = (String) selectedNode.getUserObject();

                    type = selectedNode.getNodeType();

                    if (type == IMAGEPLACEHOLDERNODE ||
                    		type == SCRIPTNODE ||
                    		(type == VOINODE && selectedNodeName.equals(VOI_EMPTY))) {
                    	return;
                    }
                    else {
                    	JMenuItem deleteMenuItem = new JMenuItem("Delete");
                        deleteMenuItem.addActionListener(new java.awt.event.ActionListener() {
                                public void actionPerformed(java.awt.event.ActionEvent e) {

                                	if (selectedNode.getNodeType() == VOINODE) {
                                		updateNode(selectedNode, VOI_EMPTY);
                                	} else {
                                		DefaultTreeModel model = (DefaultTreeModel)tree.getModel();
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
                    

                    // TODO: Apply to all script executers does not work currently.  null pointer exception on first
                    // line of updateNode()

                    // JMenuItem applyAllMenuItem = new JMenuItem("Apply to all script executers");
                    // applyAllMenuItem.addActionListener(new java.awt.event.ActionListener() {        public void
                    // actionPerformed(java.awt.event.ActionEvent e) {            applyToAllNodes(selectedNode);
                    // } // actionPerformed    } // new ActionListener                                  ); //
                    // addActionListener

                } // right mouse button clicked

            } // source equals scriptTree
            
            if (e.getSource().equals(imageList)) {
            	dropSource = IMAGE_DROP;
            //	System.err.println("SOURCE IS IMAGE_DROP");
        	} else if (e.getSource().equals(voiList)) {
         //   	System.err.println("SOURCE IS VOI_DROP");
            	dropSource = VOI_DROP;
            }
        }
        
        public void mouseReleased(MouseEvent e) {
        	if (e.getSource().equals(imageList)) {
                selectedListIndicies = ((JList) e.getSource()).getSelectedIndices();
                
                //turn off the VOI Drag option as long as more than 1 image is selected
                if (selectedListIndicies.length > 1) {
                	
                	voiList.setListData(new Vector());
                } else {
                	ScriptImage image = (ScriptImage) ((JList) e.getSource()).getSelectedValue();
                	if (image == null) {
                		return;
                	}
                	if (image.getScriptVOIs() != null) {
                		voiList.setListData(image.getScriptVOIs());
                	} else {
                		voiList.removeAll();
                		//voiList.setListData(new Vector());
                	}
                }
                
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
            //DataFlavor[] flavors = transferable.getTransferDataFlavors();
            DataFlavor dataFlavor = null;
            
            try {
            	dataFlavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType +
                ";class=java.util.ArrayList");
            } catch (ClassNotFoundException c) {
            	c.printStackTrace();
            }
                  
            String[] text = null;
            ArrayList list = null;
            
            try {
            	list = ((ArrayList)transferable.getTransferData(dataFlavor));
            	text = new String[list.size()];
            	for (int i = 0; i < text.length; i++) {
            		text[i] = (String)list.get(i);
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
            
            if (targetNodeType == SCRIPTNODE &&
            		dropSource == VOI_DROP) {
            	return;
            }
            
            
            //set up what type of node the new node will be
            if (targetNodeType == IMAGEPLACEHOLDERNODE ||
            		targetNodeType == IMAGENODE ||
            		targetNodeType == SCRIPTNODE) {
            	newNodeType = IMAGENODE;
            } else if (targetNodeType == VOINODE ||
            		targetNodeType == VOINODE) {
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
            
            //add or replace nodes one by one
            for (int i = 0; i < text.length; i++) {
            	
            	if (targetNodeType == SCRIPTNODE &&
            			dropSource == IMAGE_DROP) {
            		//going to add to as many placeholders as possible (depends on number of images dropped)
            		if (placeholderIndex < childCount) {
            			newNode = new ScriptTreeNode(text[i], newNodeType);
            			voiCount = model.getNumberOfRequiredVOIsForScriptImages()[placeholderIndex];
                		for (int j = 0; j < voiCount; j++) {
                			voiNode = new ScriptTreeNode(VOI_EMPTY, VOINODE);
                			newNode.add(voiNode);
                		}
                		
                		((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, 
                				(ScriptTreeNode)targetNode.getChildAt(placeholderIndex), 
                				((ScriptTreeNode)targetNode.getChildAt(placeholderIndex)).getChildCount());
                		placeholderIndex++;
                		
            		} else {
            			return;
            		}
            		
            	}
            	
            	else if (targetNodeType == IMAGEPLACEHOLDERNODE &&
            			dropSource == IMAGE_DROP) {
            	//	System.err.println("TARGET TYPE PLACEHOLDERNODE, SOURCE TYPE IMAGE");
            		newNode = new ScriptTreeNode(text[i], newNodeType);
            		//inserting into placeholder as new image node
            		// look for last image child and append after that
            		
            		//first add all required VOIs
            		voiCount = model.getNumberOfRequiredVOIsForScriptImages()[targetIndex];
            		for (int j = 0; j < voiCount; j++) {
            			voiNode = new ScriptTreeNode(VOI_EMPTY, VOINODE);
            			newNode.add(voiNode);
            		}
            		
            		((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, targetNode, childCount);
            		
            		//expandAll(tree, new TreePath(newNode.getPath()), true);
            	} else if (targetNodeType == IMAGENODE &&
            			dropSource == IMAGE_DROP) {
            	//	System.err.println("TARGET TYPE IMAGENODE, SOURCE TYPE IMAGE");
            			newNode = new ScriptTreeNode(text[i], newNodeType);
            			targetNode.removeAllChildren();
            			
            			targetParentIndex = targetNode.getParent().getParent().getIndex(targetNode.getParent());
            			
                        voiCount = model.getNumberOfRequiredVOIsForScriptImages()[targetParentIndex];
                		
                		for (int j = 0; j < voiCount; j++) {
                			voiNode = new ScriptTreeNode(VOI_EMPTY, VOINODE);
                			newNode.add(voiNode);
                		}
                		((DefaultTreeModel) tree.getModel()).insertNodeInto(newNode, parentNode, parentNode.getIndex(targetNode));
                        ((DefaultTreeModel) tree.getModel()).removeNodeFromParent(targetNode);
                        expandAll(tree, true);
                        dtde.acceptDrop(action);
                        dtde.dropComplete(true);
                        return;
            			
            	} else if (targetNodeType == VOINODE &&
            			dropSource == VOI_DROP) {
            //		System.err.println("TARGET TYPE VOINODE, SOURCE TYPE VOI");
            		String targetImageName = (String)((ScriptTreeNode)targetNode.getParent()).getUserObject();
            		String sourceImageName = ((JList) ((JScrollPane) getComponentByName("Images List: scroll")).getViewport().getView())
                    .getSelectedValue().toString();
            		
            		if (!(targetImageName.equalsIgnoreCase(sourceImageName))) {
            			MipavUtil.displayWarning("Source image does not contain this VOI");
            			dtde.acceptDrop(action);
                        dtde.dropComplete(true);
                        return;
            		} else {
            			updateNode(targetNode, text[i]);
            			dtde.acceptDrop(action);
                        dtde.dropComplete(true);
                        return;
            		}
            		
            	} else if (targetNodeType == IMAGENODE &&
            			dropSource == VOI_DROP) {
          //  		System.err.println("TARGET TYPE IMAGENODE, SOURCE TYPE IMAGE");
            		String targetImageName = (String)targetNode.getUserObject();
            		String sourceImageName = ((JList) ((JScrollPane) getComponentByName("Images List: scroll")).getViewport().getView())
                    .getSelectedValue().toString();
            		
            		if (!(targetImageName.equalsIgnoreCase(sourceImageName))) {
            			MipavUtil.displayWarning("Source image does not contain this VOI");
            			dtde.acceptDrop(action);
                        dtde.dropComplete(true);
                        return;
            		}
            		
            		//populate as many VOI nodes as have been dropped...if conditions allow
            		if (childCount > currentVOIIndex) {
            	//		System.err.println("Current VOI Index is: " + currentVOIIndex);
            			voiNode = (ScriptTreeNode)targetNode.getChildAt(currentVOIIndex);
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

            if (node.getNodeType() == IMAGEPLACEHOLDERNODE 
            		&& node.getChildCount() > 0) {
            	this.setForeground(Color.BLACK);
            }
            
            if (node.getNodeType() == SCRIPTNODE) {
            	setIcon(scriptIcon);
            	this.setForeground(Color.BLACK);
            } else if (node.getNodeType() == IMAGEPLACEHOLDERNODE){
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
            	if (((String)node.getUserObject()).equals(VOI_EMPTY)) {
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
    
    private class CustomCellRenderer extends JLabel 
    implements ListCellRenderer {

    	   public CustomCellRenderer () {
    	       // Don't paint behind the component
    	       setOpaque(true);
    	   }

    	   // Set the attributes of the 
    	   //class and return a reference
    	   public Component 
    	     getListCellRendererComponent(JList list,
    	         Object value, // value to display
    	         int index,    // cell index
    	         boolean iss,  // is selected
    	         boolean chf)  // cell has focus?
    	   {
    	   
    	        // Set the text and 
    	        //background color for rendering
    		   
    		   if (value instanceof ScriptImage) {
    			   if (((ScriptImage)value).isOpenedByScript()) {
    				   if (((ScriptImage)value).isMultiFile()) {
    					   setToolTipText("[multi-file]" + ((ScriptImage)value).getFileLocation());
    				   } else {
    					   setToolTipText(((ScriptImage)value).getFileLocation());
    				   }
    				   
    				   setForeground(Color.BLUE);
    			   } else {
    				   setToolTipText(((ScriptImage)value).getFileLocation());
    				   setForeground(Color.BLACK);
    			   }
    			   setText(((ScriptImage)value).getImageName());
    			   
    			   
    			   
    		   } else if (value instanceof ScriptVOI) {
    			   if (((ScriptVOI)value).getVoiFileLocation() == null) {
    				   setForeground(Color.BLACK);
    			   } else {
    				   setForeground(Color.BLUE);
    				   setToolTipText(((ScriptVOI)value).getVoiFileLocation());
    			   }
    			   
    			   setText(((ScriptVOI)value).toString());
    			   
    		   }
    		      	       

    	         // Set a border if the 
    	         //list item is selected
    	        if (iss) {
    	        	setBorder(BorderFactory.createLineBorder(
    	              Color.GRAY, 1));
    	        	setBackground(Color.LIGHT_GRAY);
    	        } else {
    	            setBorder(null);
    	            setBackground(list.getBackground());
    	        }

    	        return this;
    	    }
    	 }
    
} // class
