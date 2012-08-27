package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.awt.image.PixelGrabber;
import java.util.Enumeration;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTree;
import javax.swing.SwingConstants;
import javax.swing.border.Border;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmVOILogicalOperations;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIStatisticList;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.model.structures.event.VOIEvent;
import gov.nih.mipav.model.structures.event.VOIListener;
import gov.nih.mipav.model.structures.event.VOIVectorEvent;
import gov.nih.mipav.model.structures.event.VOIVectorListener;
import gov.nih.mipav.view.JPanelTreeController;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.VOIFrameNode;
import gov.nih.mipav.view.VOIGroupNode;
import gov.nih.mipav.view.VOIHandlerInterface;
import gov.nih.mipav.view.VOIContourNode;
import gov.nih.mipav.view.VOIOrientationNode;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;


/**
 * @author pandyan
 * 
 * This dialog and utility allows for logical operations of VOIs
 *
 */
public class JDialogVOILogicalOperations extends JDialogScriptableBase implements AlgorithmInterface, VOIStatisticList,
VOIVectorListener, TreeSelectionListener, ActionDiscovery {
	
	
	 /** image and cloned image */
    protected ModelImage image, clonedImage;
    
    protected ViewUserInterface userInterface;
    
    /** Tabbed pane that holds all components. */
    protected JTabbedPane everything;
    
    /** Operator to provide listener access... could be done by /this/ */
    private VOIHighlighter highlighter;
    
    /** Panel to push/pull VOIs from full list to selectable list. */
    private JPanelAddRemoveVOI addremove;
    
    /** algorithm **/
    private AlgorithmVOILogicalOperations alg;
    
    /** panels **/
    private JPanel logicalOptionsPanel, imageOptionsPanel;

    /** radio button **/
    private JRadioButton and;

    /** radio button **/
    private JRadioButton or;

    /** radio button **/
    private JRadioButton xor;

    /** radio button **/
    private JRadioButton createVoiImage;
    
    /** radio button **/
    private JRadioButton createMaskImage;

    /** flag indicating whether output should be VOI image or mask image **/
    private boolean doVOIImage = false;
    
    /** DOCUMENT ME! */
    private DefaultMutableTreeNode sourceRoot, selectedRoot;
    
    /** The tree of VOIs, composed of an image with children VOIs */
    private DefaultTreeModel sourceVoiModel, selectedVOIModel;
    
    /** The graphical representation of voiModel */
    private JTree sourceVoiTree;
    
    private JTree selectedVoiTree;
    
    /** DOCUMENT ME! */
    private static Icon ICON_POLYGON = MipavUtil.getIcon("polygon.gif");

    /** DOCUMENT ME! */
    private static Icon ICON_POLYLINE = MipavUtil.getIcon("polyline.gif");

    /** DOCUMENT ME! */
    private static Icon ICON_POINT = MipavUtil.getIcon("pointROI.gif");

    /** DOCUMENT ME! */
    private static Icon ICON_LINE = MipavUtil.getIcon("linear.gif");

    /** DOCUMENT ME! */
    private static Icon ICON_MEDICAL_FRAME = MipavUtil.getIcon("med_frame.gif");
    /** DOCUMENT ME! */
    private static Icon ICON_X_AXIS = MipavUtil.getIcon("xalign.gif");
    /** DOCUMENT ME! */
    private static Icon ICON_Y_AXIS = MipavUtil.getIcon("yalign.gif");
    /** DOCUMENT ME! */
    private static Icon ICON_Z_AXIS = MipavUtil.getIcon("zalign.gif");
    
    /** DOCUMENT ME! */
    private static Icon ICON_PROTRACTOR = MipavUtil.getIcon("protractor.gif");
	
    
    private Border frameBorder= BorderFactory.createCompoundBorder(BorderFactory.createRaisedBevelBorder(),
            BorderFactory.createLoweredBevelBorder());
    
    private JScrollPane jsp;
    
    private boolean treeSelectionChange = false;
    private boolean updateTree = false;
    
    protected VOIHandlerInterface voiHandler;
    
    private VOI selectedVOI;
    
    int index = 0;
    
    int logicalOperation;
    
    ViewVOIVector processList;
	
	
	
	/**
	 * constructor
	 * @param voiList
	 */
	public JDialogVOILogicalOperations(VOIHandlerInterface voiHandler,VOIVector voiList) {
        super(ViewUserInterface.getReference().getMainFrame(), false);

        image = ViewUserInterface.getReference().getActiveImageFrame().getActiveImage();
        this.voiHandler = voiHandler;

        
        buildDialog(voiList);


       
    }
	
	/**
     * Clean up some things done by the dialog which may affect other parts of MIPAV.
     */
    protected void cleanUpAndDispose() {

        // make sure all voi listeners are removed from the image's list, if that isn't done problems happen when
        // the image is serialized (e.g. if it's cloned)
        for (int i = 0; i < image.getVOIs().size(); i++) {
            image.getVOIs().VOIAt(i).removeVOIListener(highlighter);
        }

        image.getVOIs().removeVectorListener(this);

        dispose();
    }

	
	
	/**
	 * Builds the dialog
	 * @param voiList
	 */
	protected void buildDialog(final VOIVector voiList) {
        setTitle("Calculate Logical Operations on VOI groups");

        this.userInterface = ViewUserInterface.getReference();
        
        
        everything = new JTabbedPane(SwingConstants.TOP);
        everything.setFont(MipavUtil.font12B);
        everything.insertTab("VOI selection", null, buildVOIPanel(voiList), // we must store this panel so we can
                // create a new listing later
                "Choose VOIs and statistics file", JDialogVOIStatistics.VOI_TAB);


        
        getContentPane().add(everything, BorderLayout.CENTER);
        getContentPane().add(buildOKCancelPanel(), BorderLayout.SOUTH); // build OK/Cancel button Panel

        pack();
        setSize(800, 500); // decent size??
        
        
	}
	
	
	
	
	

    /**
     * creates the panel which consists of the OKAY button and the Cancel button.
     * 
     */
    protected JPanel buildOKCancelPanel() {
        final JPanel ocp = new JPanel(); // flow layout

        buildOKButton();
        OKButton.setText("OK");
        ocp.add(OKButton);

        buildCancelButton();
        cancelButton.setText("Close");
        cancelButton.setActionCommand("Cancel");
        ocp.add(cancelButton);

        buildHelpButton();
        helpButton.setText("Help");
        ocp.add(helpButton);

        return ocp;
    }
	
	 /**
     * un-implemented.
     * 
     * @param voiEvent DOCUMENT ME!
     */
    public void vectorSelected(final VOIVectorEvent voiEvent) {}
	
	
	
    /**
     * resets the volumes list to the current VOIVector. adds the highlighter to the new VOI.
     * 
     * @param voiEvent DOCUMENT ME!
     */
    public void addedVOI(final VOIVectorEvent voiEvent) {
        final VOIVector voiList = (VOIVector) voiEvent.getSource();
        final Vector<VOI> volumesVector = new Vector<VOI>();

        for (int i = 0; i < voiList.size(); i++) {

            if (voiList.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                volumesVector.add(voiList.elementAt(i));
            }
        }

        //volumesList.setListData(volumesVector);
        voiEvent.getVOI().addVOIListener(highlighter);
    }
    
    
    
    /**
     * resets the volumes list to the current VOIVector. removes the highlighter from the removed VOI.
     * 
     * @param voiEvent DOCUMENT ME!
     */
    public void removedVOI(final VOIVectorEvent voiEvent) {
        final VOIVector voiList = (VOIVector) voiEvent.getSource();
        final Vector<VOI> volumesVector = new Vector<VOI>();

        for (int i = 0; i < voiList.size(); i++) {

            if (voiList.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                volumesVector.add(voiList.elementAt(i));
            }
        }

        //volumesList.setListData(volumesVector);

        /*
         * we cannot delete VOIs out of the selected VOI list if there are no more VOIs in the image. --
         * voiEvent.getSource() is null when 'removeAll' is called. getVOI is to return, specifically, the new VOI that
         * has changed. Since all VOIs are now null, we must recognise that the VOI that is new is the empty VOI. (one
         * of these a-ha! moments. silly comments left undone.)
         */
        if (voiEvent.getVOI() == null) {
            return;
        }

        if (voiEvent.getVOI().getCurveType() == VOI.CONTOUR) {
            voiEvent.getVOI().removeVOIListener(highlighter);

            // ensuring that the selected VOI is not in
            // the selected list.
            //selectedList.setSelectedValue(voiEvent.getVOI(), true);
            addremove.performDelete();
        }
    }

	
	
	/**
     * Creates the panel holding the directory tree.
     * 
     * @param VOIlist DOCUMENT ME!
     * 
     * @return Panel.
     */
    private JPanel buildSourceListingPanel(final VOIVector VOIlist) {
        final JPanel srctreep = new JPanel(new BorderLayout());
        /*final Vector volumesVector = new Vector();
        highlighter = new VOIHighlighter();

        for (int i = 0; i < VOIlist.size(); i++) {

            if (VOIlist.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                volumesVector.add(VOIlist.elementAt(i));

                // add a listener to each VOI so we know about selection.
                VOIlist.VOIAt(i).addVOIListener(highlighter);
            }
        }

        volumesList.setListData(volumesVector);
        volumesList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        volumesList.addListSelectionListener(highlighter);*/

        jsp = new JScrollPane(buildVOITree());
        srctreep.add(jsp, BorderLayout.CENTER);

        // now let's listen to this vector:
        //VOIlist.addVectorListener(this);

        return srctreep;
    }
    
    
    
    
    @SuppressWarnings("unchecked")
    public void updateTree() {
        if (treeSelectionChange) {
            treeSelectionChange = false;
            return;
        }
        updateTree = true;
        if (this.isVisible()) {
        	sourceRoot.removeAllChildren();

            ViewVOIVector VOIs = image.getVOIs();
            Enumeration<VOI> e = VOIs.elements();

            VOI tempVOI = null;

            VOIGroupNode currentNode = null;
            Vector<VOIBase> curves = null;

            Enumeration<VOIBase> voiEnum = null; 
            VOIBase voiBase = null;

            Enumeration<VOIFrameNode> voiNodeEnum = null;
            VOIContourNode currentVOINode = null;
            Enumeration<TreeNode> voiFrameEnum = null;

            Vector<TreePath> treePaths = new Vector<TreePath>();

            TreeNode tempNode = null;

            // iterate through all VOIs
            while (e.hasMoreElements()) {
                tempVOI = e.nextElement();

                // create VOI group node (for VOI)
                currentNode = new VOIGroupNode(tempVOI,image.getExtents());

                // check to see if the current VOI is the VOI shown in this dialog
                // or if the VOI isActive (can have multiple selections on tree)
                if (tempVOI.isActive())
                {

                    // add a new tree path so the VOI node is selected
                    treePaths.addElement(new TreePath(new Object[] { sourceRoot, currentNode }));

                    curves = tempVOI.getCurves();

                    // look through curves to find which of the VOI's VOIBases (contours etc)
                    // are active
                    voiEnum = curves.elements();
                    while (voiEnum.hasMoreElements()) {
                        voiBase = voiEnum.nextElement();

                        // check to see if the VOIBase is active
                        if (voiBase.isActive())
                        {

                            voiFrameEnum = currentNode.children();

                            while (voiFrameEnum.hasMoreElements()) {

                                tempNode = (TreeNode) voiFrameEnum.nextElement();

                                if (tempNode instanceof VOIOrientationNode) {

                                    voiNodeEnum = tempNode.children();

                                    // find the child that matches this selected contour
                                    while (voiNodeEnum.hasMoreElements()) {
                                        
                                        VOIFrameNode currentFrameNode = voiNodeEnum.nextElement();
                                        Enumeration<VOIContourNode> voiFrameEnum2 = currentFrameNode.children();
                                        
                                        // find the child that matches this selected contour
                                        while (voiFrameEnum2.hasMoreElements()) {
                                            currentVOINode = (VOIContourNode) voiFrameEnum2.nextElement();

                                            if (currentVOINode.getVOI().equals(voiBase)) {
                                                treePaths.addElement(new TreePath(new Object[] {
                                                		sourceRoot, currentNode, tempNode, currentFrameNode,
                                                        currentVOINode
                                                }));
                                            }
                                        }
                                    }

                                }
                            }

                        }
                    }
                }

                sourceRoot.add(currentNode);
            }
            sourceVoiModel.reload();

            if (treePaths.size() > 0) {
                TreePath[] tPaths = new TreePath[treePaths.size()];

                for (int i = 0; i < tPaths.length; i++) {
                    tPaths[i] = treePaths.elementAt(i);
                }
                sourceVoiTree.setSelectionPaths(tPaths);

                for (int i = 0; i < tPaths.length; i++) {
                    sourceVoiTree.expandPath( tPaths[i] );
                }
            } else {
                TreePath path = new TreePath(sourceRoot);
                sourceVoiTree.setSelectionPath(path);
                sourceVoiTree.expandPath(path);
            }
            jsp.validate();
        }

        updateTree = false;
    }
    
    
    
    
    
    /**
     * DOCUMENT ME!
     */
    protected JTree buildVOITree() {

        ViewVOIVector VOIs = image.getVOIs();

        sourceRoot = new DefaultMutableTreeNode(image.getImageName());
        sourceVoiModel = new DefaultTreeModel(sourceRoot);

        Enumeration<VOI> e = VOIs.elements();

        VOI currentVOI = null;

        int index = 0;

        while (e.hasMoreElements()) {
            currentVOI = e.nextElement();
            sourceVoiModel.insertNodeInto(new VOIGroupNode(currentVOI,image.getExtents()), sourceRoot, index);
            //voiModel.insertNodeInto(new VOIGroupNode(currentVOI), root, index);
            index++;
        }

        sourceVoiTree = new JTree(sourceVoiModel);
        sourceVoiTree.setCellRenderer(new VOITreeRenderer());

        sourceVoiTree.setFont(MipavUtil.font12);
        sourceVoiTree.addTreeSelectionListener(this);
        
        


       /* voiTreePane = new JScrollPane(voiTree, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                      JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        voiTreePane.setPreferredSize(new Dimension(100,300));*/
        
        return sourceVoiTree;
    }
    
    
    
    
    
    
    
	
	/**
     * creates the source panel which consists of the directory line, the browse button, and a check box approving the
     * anonymize in sub-directories.
     * 
     * @param VOIlist DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private JPanel buildSourcePanel(final VOIVector VOIlist) {
        final JPanel srcp = new JPanel(new GridLayout(1, 2));
        srcp.setBorder(buildTitledBorder("VOI group list"));

        srcp.add(buildSourceListingPanel(VOIlist), BorderLayout.CENTER); // list of VOIs in the image.
        srcp.add(buildSelectedListing(), BorderLayout.EAST); // list of selected items

        return srcp;
    }
    
    
    /**
     * creates the visual display in which to list all selected directories in the directory tree. The panel is 240
     * pixels wide though that is <i>supposed</i> to be the minimum size
     * 
     * @return the panel which is to hold the list of selected items
     */
    private JPanel buildSelectedListing() {

        // define an outside panel to hold all these components.
        final JPanel selp = new JPanel(new BorderLayout());
        selp.add(Box.createHorizontalStrut(240), BorderLayout.NORTH); // width of text area. seems to start out very
        // skinny.
        
        selectedRoot = new DefaultMutableTreeNode(image.getImageName());
        
        selectedVOIModel = new DefaultTreeModel(selectedRoot);

        selectedVoiTree = new JTree(selectedVOIModel);
        selectedVoiTree.setCellRenderer(new VOITreeRenderer());

        selectedVoiTree.setFont(MipavUtil.font12);
        selectedVoiTree.addTreeSelectionListener(this);
        
        
        selp.add(new JScrollPane(selectedVoiTree), BorderLayout.CENTER);
        
        
        

        // this list to hold things so that they may be selectable/removable
        // panel to hold list access.
       /* selectedList.setListData(new Vector()); // = new JList();
        selp.add(new JScrollPane(selectedList), BorderLayout.CENTER);*/

        // build default arrowpanel
        if (addremove == null) {
            addremove = new JPanelAddRemoveVOI();
            addremove.setLeftTree(sourceVoiTree);
            addremove.setRightTree(selectedVoiTree);

            /*volumesList.addListSelectionListener(addremove);
            selectedList.addListSelectionListener(addremove);*/

            selp.add(addremove, BorderLayout.WEST);
        }

        return selp;
    }
	
	
	/**
     * creates the source panel for the VOI tab which consists of the directory line, the browse button, and a check box
     * approving the anonymize in sub-directories. Also includes the file-format selection for the output file.
     * 
     * @param VOIlist DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildVOIPanel(final VOIVector VOIlist) {
        final JPanel imagePanel = new JPanel(new BorderLayout());

        // we must store sourcePanel so we can create a new directory listing later
        imagePanel.add(buildSourcePanel(VOIlist), BorderLayout.CENTER);

        final JPanel optionsPanel = new JPanel(new GridBagLayout());
        
        GridBagConstraints gbc = new GridBagConstraints();

        logicalOptionsPanel = new JPanelLogicalOptions();
        imageOptionsPanel = new JPanelImageOptions();
        gbc.gridx = 0;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        optionsPanel.add(logicalOptionsPanel, gbc);
        gbc.gridx = 1;
        optionsPanel.add(imageOptionsPanel, gbc);

        imagePanel.add(optionsPanel, BorderLayout.SOUTH);

        return imagePanel;
    }
	
	

	/**
	 * 
	 */
	protected void callAlgorithm() {
		Object root = selectedVOIModel.getRoot();
        
        int voiCount = selectedVOIModel.getChildCount(root);
        
        processList = new ViewVOIVector(voiCount);
        
        for(int i=0;i<voiCount;i++) {
        	VOI voi = new VOI((short)i,"voi" + i);
        	
        	VOIGroupNode groupNode = (VOIGroupNode)selectedVOIModel.getChild(root, i);
        	
        	int orientationCount = groupNode.getChildCount();
        	
        	for(int k=0;k<orientationCount;k++) {
        		try {
	        		VOIOrientationNode orientationNode = (VOIOrientationNode)selectedVOIModel.getChild(groupNode, k);
        		
	        		
	        		int frameCount = orientationNode.getChildCount();
	        		
	        		for(int m=0;m<frameCount;m++) {
	        			
	        			VOIFrameNode frameNode = (VOIFrameNode)selectedVOIModel.getChild(orientationNode, m);
	        			
	        			int contourCount = frameNode.getChildCount();
	        			
	        			for(int c=0;c<contourCount;c++) {
	        				
	        				VOIContourNode contourNode = (VOIContourNode)selectedVOIModel.getChild(frameNode, c);
	        				VOIBase contour = contourNode.getVOI();
	        				
	        				voi.importCurve(contour);
	        				
	        			}
	        			
	        			
	        		}
        		}
        		catch(ClassCastException e) {
        			VOIFrameNode frameNode = (VOIFrameNode)selectedVOIModel.getChild(groupNode, 0);
        			
        			int contourCount = frameNode.getChildCount();
        			
        			for(int c=0;c<contourCount;c++) {
        				
        				VOIContourNode contourNode = (VOIContourNode)selectedVOIModel.getChild(frameNode, c);
        				VOIBase contour = contourNode.getVOI();
        				
        				voi.importCurve(contour);
        				
        			}	
        		}
        		
        		
        	}
        	
        	
        	
        	
        	
        	 processList.add(voi);
        	
        	
        }
		

        
        
        doVOIImage = false;
        
        if(and.isSelected()) {
        	logicalOperation = AlgorithmVOILogicalOperations.ADD;
        }else if(or.isSelected()) {
        	logicalOperation = AlgorithmVOILogicalOperations.OR;
        }else {
        	logicalOperation = AlgorithmVOILogicalOperations.XOR;
        }
        
        if(createVoiImage.isSelected()) {
        	doVOIImage = true;
        }
        
        
        
        clonedImage = (ModelImage)(image.clone());
        
        clonedImage.unregisterAllVOIs();
        
        
        alg = new AlgorithmVOILogicalOperations(clonedImage, processList,logicalOperation, doVOIImage);
        
        alg.addListener(this);

        setVisible(false);
        
        if (isRunInSeparateThread()) {
            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
        	alg.run();
        } 

	}

	/**
	 * 
	 */
	protected void setGUIFromParams() {
		setLogicalOperation(Integer.valueOf(scriptParameters.getParams().getString("operation")).intValue());
		setDoVOIImage(Boolean.valueOf(scriptParameters.getParams().getString("doVOIImg")).booleanValue());
		image = scriptParameters.retrieveInputImage();
		processList = image.getVOIs();
		clonedImage = (ModelImage)(image.clone());
        clonedImage.unregisterAllVOIs();

	}

	/**
	 * 
	 */
	protected void storeParamsFromGUI() throws ParserException {

		if(and.isSelected()) {
        	logicalOperation = AlgorithmVOILogicalOperations.ADD;
        	
        }else if(or.isSelected()) {
        	logicalOperation = AlgorithmVOILogicalOperations.OR;
        }else {
        	logicalOperation = AlgorithmVOILogicalOperations.XOR;
        }
		scriptParameters.getParams().put(ParameterFactory.newParameter("operation", logicalOperation));
        
		boolean doVOIImg = false;
        if(createVoiImage.isSelected()) {
        	doVOIImg = true;
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("doVOIImg", doVOIImg));
        
        
		

	}

	/**
	 * 
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(doVOIImage) {

	        VOIVector kVOIs = clonedImage.getVOIs();
	        for ( int i = 0; i < kVOIs.size(); i++ )
	        {
	            kVOIs.elementAt(i).setAllActive(true);
	        }
	        clonedImage.groupVOIs();
			
			
			new ViewJFrameImage(clonedImage);

			
		}else {
			ModelImage finalMaskImage = ((AlgorithmVOILogicalOperations)algorithm).getFinalMaskImage();
			new ViewJFrameImage(finalMaskImage);
		}
		cleanUpAndDispose();

	}

	/**
	 * 
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equals("OK")) {
			/*if(selectedList.getModel().getSize() == 0) {
				MipavUtil.displayError("You must select at least 1 VOI");
				return;
			}*/
	           
	           if(selectedVoiTree.getRowCount() == 1) {
	        	   MipavUtil.displayError("You must select at least 1 VOI");
	        	   return;
	           }
			callAlgorithm();
		}else if(command.equals("Cancel")) {
			if(clonedImage != null) {
				 clonedImage.disposeLocal();
				 clonedImage = null;
			 }
			cleanUpAndDispose();
		} else {
            super.actionPerformed(e);
        }

	}
	
	
	 public void windowClosing(WindowEvent event) {
		 if(clonedImage != null) {
			 clonedImage.disposeLocal();
			 clonedImage = null;
		 }
		 cleanUpAndDispose();
		 
	 }
	 
	  private void printTree( TreeModel model, Object parent )
	    {
	        if ( model.isLeaf(parent) && parent instanceof VOIContourNode )
	        {
	            VOIBase contour = ((VOIContourNode)parent).getVOI();
	            if ( contour != null )
	            {
	                contour.setActive(false);
	            }
	            return;
	        }
	        int childCount = model.getChildCount(parent);
	        for ( int i = 0; i < childCount; i++ )
	        {
	            printTree( model, model.getChild(parent,i) );
	        }
	    }
	  
	  
	  
	 
	  /**
	     * Updates the ViewJFrameImage when a VOI/contour is selected.
	     *
	     * @param  e  TreeSelectionEvent
	     */
	    public void valueChanged(TreeSelectionEvent e) {
	        if ( updateTree )
	        {
	            return;
	        }
	        TreeModel model = sourceVoiTree.getModel();
	        if ( !updateTree )
	        {
	            printTree( model, model.getRoot() );
	        }
	        
	        TreePath leadPath = e.getNewLeadSelectionPath();

	        if (leadPath != null) {
	            Object[] leadObjects = leadPath.getPath();
	            //int curveIndex = 0;

	            if (leadObjects[leadObjects.length - 1] instanceof VOIContourNode) {
	                VOIBase leadBase = ((VOIContourNode) leadObjects[leadObjects.length - 1]).getVOI();
	                //VOI leadVOI = ((VOIGroupNode)((VOIFrameNode) ((VOINode) leadObjects[leadObjects.length - 1]).getParent()).getParent()).getVOIgroup();

	                if ((image.getNDims() > 2)) {
	                    voiHandler.setCenter(leadBase.getGeometricCenter(), true );
	                    //System.err.println( "frameFollowsSelection " + leadBase );
	                    leadBase.setActive(true);
	                }

	                //updateContourPane(leadBase);
	                //updateVOI(leadBase.getGroup(), image);

	            } else if (leadObjects[leadObjects.length - 1] instanceof VOIFrameNode) {
	                //curveIndex = ((VOIFrameNode) leadObjects[leadObjects.length - 1]).getFrameNumber();

	                if ((image.getNDims() > 2)) {
	                    //voiHandler.setSlice(curveIndex);
	                }
	            }

	        }
	        treeSelectionChange = true;
	    }
	
	
	
	
	    /**
	     * Updates the dialog based on the VOI passed in.
	     *
	     * @param  _voi  VOI whose properties we want to calculate.
	     * @param  img   Image where voi is to be updated
	     */
	    public void updateVOI(VOI _voi, ModelImage img) {
	    	selectedVOI = _voi;
	    	ViewUserInterface.getReference().setUseVOIName(false);
	    	
	    	
	    }
	
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////    INNER CLASSES    ///////////////////////////////
	 
	  private class VOITreeRenderer extends DefaultTreeCellRenderer {

	        /** Use serialVersionUID for interoperability. */
	        private static final long serialVersionUID = -4107179145193872844L;

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

	            if (value instanceof VOIGroupNode) {

	                setIcon(null);

	                int type = ((VOIGroupNode) value).getVOIgroup().getCurveType();

	                Icon typeIcon = null;

	                switch (type) {

	                    case VOI.POLYLINE:
	                        typeIcon = ICON_POLYLINE;
	                        break;

	                    case VOI.CONTOUR:
	                        typeIcon = ICON_POLYGON;
	                        break;

	                    case VOI.LINE:
	                        typeIcon = ICON_LINE;
	                        break;

	                    case VOI.POINT:
	                        typeIcon = ICON_POINT;
	                        break;

	                    case VOI.PROTRACTOR:
	                        typeIcon = ICON_PROTRACTOR;
	                        break;

	                    default:
	                        setIcon(null);
	                }

	                int rgb = ((VOIGroupNode) value).getVOIgroup().getColor().getRGB();
	                int black = Color.black.getRGB();

	                if (typeIcon != null) {
	                    ImageIcon ico = (ImageIcon) typeIcon;
	                    int imageWidth = ico.getIconWidth();
	                    int imageHeight = ico.getIconHeight();

	                    int[] pixels = new int[imageWidth * imageHeight];
	                    PixelGrabber pg = new PixelGrabber(ico.getImage(), 0, 0, imageWidth, imageHeight, pixels, 0,
	                                                       imageWidth);

	                    try {
	                        pg.grabPixels();
	                    } catch (InterruptedException e) {
	                        Preferences.debug("JIMI: Interrupted waiting for pixels!" + "\n");

	                        return null;
	                    }

	                    BufferedImage image2 = new BufferedImage(ico.getIconWidth() + 15, ico.getIconHeight(),
	                                                             BufferedImage.TYPE_INT_ARGB);

	                    for (int y = 0; y < imageHeight; y++) {

	                        for (int x = 0; x < imageWidth; x++) {
	                            image2.setRGB(x, y, pixels[(y * imageWidth) + x]);
	                        }
	                    }


	                    // draw black border around color box
	                    for (int i = ico.getIconWidth() + 3; i < (image2.getWidth() - 3); i++) {

	                        for (int j = 0; j < 2; j++) {
	                            image2.setRGB(i, j, black);
	                        }

	                        for (int j = image2.getHeight() - 2; j < image2.getHeight(); j++) {
	                            image2.setRGB(i, j, black);
	                        }
	                    }

	                    for (int j = 0; j < image2.getHeight(); j++) {

	                        for (int i = ico.getIconWidth() + 3; i < (ico.getIconWidth() + 5); i++) {
	                            image2.setRGB(i, j, black);
	                        }

	                        for (int i = image2.getWidth() - 5; i < (image2.getWidth() - 3); i++) {
	                            image2.setRGB(i, j, black);
	                        }

	                    }

	                    // draw color
	                    for (int i = ico.getIconWidth() + 5; i < (image2.getWidth() - 5); i++) {

	                        for (int j = 2; j < (image2.getHeight() - 2); j++) {
	                            image2.setRGB(i, j, rgb);
	                        }
	                    }

	                    setIcon(new ImageIcon(image2));

	                } else {
	                    BufferedImage image = new BufferedImage(9, 26, BufferedImage.TYPE_INT_ARGB);

	                    for (int i = 2; i < 7; i++) {

	                        for (int j = 4; j < 24; j++) {
	                            image.setRGB(i, j, rgb);
	                        }
	                    }

	                    // draw black border
	                    for (int i = 0; i < 9; i++) {

	                        for (int j = 2; j < 4; j++) {
	                            image.setRGB(i, j, black);
	                        }

	                        for (int j = 24; j < 26; j++) {
	                            image.setRGB(i, j, black);
	                        }
	                    }

	                    for (int j = 2; j < 26; j++) {

	                        for (int i = 0; i < 2; i++) {
	                            image.setRGB(i, j, black);
	                        }

	                        for (int i = 7; i < 9; i++) {
	                            image.setRGB(i, j, black);
	                        }
	                    }

	                    setIcon(new ImageIcon(image));
	                }

	                // ImageIcon ico = new ImageIcon(image);
	                // setIcon(ico);


	                setBorder(null);
	                setFont(MipavUtil.font12);
	            } else if (value instanceof VOIFrameNode) {
	                setBorder(frameBorder);
	                setFont(MipavUtil.font10);
	                setIcon(null);

	            } else if (value instanceof VOIContourNode) {
	                setIcon(null);
	                setBorder(null);
	                setFont(MipavUtil.font12);
	            } else if (value instanceof VOIOrientationNode) {
	            	if(((VOIOrientationNode)value).getName().equals("X Plane")) {
	            		setIcon(ICON_X_AXIS);
	            	}else if(((VOIOrientationNode)value).getName().equals("Y Plane")) {
	            		setIcon(ICON_Y_AXIS);
	            	}else if(((VOIOrientationNode)value).getName().equals("Z Plane")) {
	            		setIcon(ICON_Z_AXIS);
	            	}
	            	
	                setFont(MipavUtil.font12);
	                setBorder(null);
	            } else {

	                // setForeground(Color.white);
	                setIcon(ICON_MEDICAL_FRAME);
	                setFont(MipavUtil.font12);
	                setBorder(null);
	                // setIcon(null);
	            }

	            return this;
	        }
	    }
	
	
	
	
	
	/**
     * controllas the lists between left and right side.
     */
    public class JPanelAddRemoveVOI extends JPanelTreeController {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -2076771728355899710L;

        /**
         * Sets the Add/Remove VOI panel to Y-Axis layout and with images.
         */
        JPanelAddRemoveVOI() {
            super();
            setBackArrowEnabled(false);
            setBackArrowVisble(false);
            setDeleteEnabled(true);
        }
        
        protected void copySelected(JTree a, JTree b) {

   
            int pathCount;
            
            


            // ignore if there are no selections made.
            if (a.isSelectionEmpty()) {
                return;
            }

            // Next, copy in the selected values at the bottom.
            TreePath[] selectedValues = a.getSelectionModel().getSelectionPaths();
            pathCount = ((TreePath)selectedValues[0]).getPathCount();
            if(pathCount == 1) {
            	return;
            }
            
            VOIGroupNode aGroupNode;
            aGroupNode = (VOIGroupNode)(((TreePath)selectedValues[0]).getPathComponent(1));
            
            
        	

            if(pathCount == 2 || pathCount == 3 || pathCount == 4) {
            	selectedVOI = aGroupNode.getVOIgroup();
 
            }
            
            // Lastly, copy the total tree into the tree to view and clean up.
            //b.addSelectionPaths(selectedValues);

            
            Object root = selectedVOIModel.getRoot();
            
            int childCountFromRoot = selectedVOIModel.getChildCount(root);
            boolean found = false;
            VOIGroupNode foundGroupNode = null;
            for(int k=0;k<childCountFromRoot;k++) {
            	foundGroupNode = (VOIGroupNode)selectedVOIModel.getChild(root, k);
            	
            	if(foundGroupNode.getName().equals(selectedVOI.getName())) {
            		found = true;
            		break;
            	}
            	
            	
            }
            
            if(!found) {
            	VOIGroupNode groupNode = new VOIGroupNode(selectedVOI,image.getExtents());
	            selectedVOIModel.insertNodeInto(groupNode, selectedRoot, index);
	            index++;

	            if(pathCount !=  2) {
	            	//start with a blank slate (remove all the slice nodes) since only 1 contour was selected
	            	  
		            
		            int childCountFromGroupNode = selectedVOIModel.getChildCount(groupNode);
		            for(int k=0;k<childCountFromGroupNode;k++) {
		            	VOIOrientationNode orientationNode = (VOIOrientationNode)selectedVOIModel.getChild(groupNode, k);
		            	int childCountFromOrientationNode = selectedVOIModel.getChildCount(orientationNode);
		            	if(childCountFromOrientationNode > 0) {
			            	for(int m=childCountFromOrientationNode-1;m>=0;m--) {
			            		
			            		VOIFrameNode frameNode = (VOIFrameNode)selectedVOIModel.getChild(orientationNode, m);
			            		selectedVOIModel.removeNodeFromParent(frameNode);
			            	}
		            	}
		            	
		            }
		            selectedVOIModel.reload();
		          
		            
		            
		            
		            
		            
		            
		            childCountFromRoot = selectedVOIModel.getChildCount(root);
		            
		            for(int k=0;k<childCountFromRoot;k++) {
		            	foundGroupNode = (VOIGroupNode)selectedVOIModel.getChild(root, k);
		            	
		            	if(foundGroupNode.getName().equals(selectedVOI.getName())) {
		            		found = true;
		            		break;
		            	}
		            	
		            	
		            }
		            
		            VOIOrientationNode aOrientationNode,bOrientationNode;
		        	VOIFrameNode aFrameNode,bFrameNode;
		        	//VOIFrameNode bFrameNode2;
		        	VOIContourNode aNode,bNode;
		        	// VOINode bNode2;
		        	aFrameNode = null;
		        	bFrameNode = null;
		        	aNode = null;
		        	bNode = null;
		        	
		        	
		        	aOrientationNode = (VOIOrientationNode)(((TreePath)selectedValues[0]).getPathComponent(2));
		        	
		        	if(pathCount == 3) {

		        	}else if(pathCount == 4) {
		        		aFrameNode = (VOIFrameNode)(((TreePath)selectedValues[0]).getPathComponent(3));
		        		bFrameNode = (VOIFrameNode)aFrameNode.clone();

		        	}else if(pathCount == 5) {
		        		aFrameNode = (VOIFrameNode)(((TreePath)selectedValues[0]).getPathComponent(3));
			        	aNode = (VOIContourNode)(((TreePath)selectedValues[0]).getPathComponent(4));
			        	bFrameNode = (VOIFrameNode)aFrameNode.clone();
			        	bNode = (VOIContourNode)aNode.clone();
		        	}
		        	
		        			
		        	//bFrameNode2 = null;	
	            	
	            	int childCount1 = foundGroupNode.getChildCount();
	            	for(int k=0;k<childCount1;k++) {
	            		bOrientationNode = (VOIOrientationNode)selectedVOIModel.getChild(foundGroupNode, k);
	            		if(aOrientationNode.getName().equals(bOrientationNode.getName())) {
	            			/*int childCount2 = bOrientationNode.getChildCount();
	            			if(childCount2 > 0) {
	            				boolean found2 = false;
	            				for(int m=0;m<childCount2;m++) {
	            					bFrameNode2 = (VOIFrameNode)selectedVOIModel.getChild(bOrientationNode, m);
	            					if(bFrameNode2.getName().equals(aFrameNode.getName())) {
	            						found2 = true;
	            						break;
	            					}
	            				}
	            				if(found2) {
	            					int childCount3 = bFrameNode2.getChildCount();
	            					boolean found3 = false;
	            					for(int m=0;m<childCount3;m++) {
	            						bNode2 = (VOINode)selectedVOIModel.getChild(bFrameNode2, m);
	            						if(bNode2.getName().equals(aNode.getName())) {
	                						found3 = true;
	                						break;
	                					}
	            					}
	            					if(!found3) {
	            						selectedVOIModel.insertNodeInto(bNode, bFrameNode2, childCount3);
	            					}
	            					
	            				}else {
	            					selectedVOIModel.insertNodeInto(bFrameNode, bOrientationNode, 0);
	                				selectedVOIModel.insertNodeInto(bNode, bFrameNode, 0);
	            				}
	            				
	            				
	            			}else {*/
	            				if(pathCount == 3) {
	            					int frameCount = aOrientationNode.getChildCount();
	            					int c1=0;
	            					for(int m=0;m<frameCount;m++) {
	            						aFrameNode = (VOIFrameNode)selectedVOIModel.getChild(aOrientationNode, m);
	            						bFrameNode = (VOIFrameNode)aFrameNode.clone();
	            						
	            						selectedVOIModel.insertNodeInto(bFrameNode, bOrientationNode, c1);
	            						
	            						int contourCount = aFrameNode.getChildCount();
	            						
	            						int c=0;
		            					for(int i=0;i<contourCount;i++) {
		            						
		            						selectedVOIModel.insertNodeInto((VOIContourNode)((VOIContourNode)selectedVOIModel.getChild(aFrameNode,i)).clone(), bFrameNode, c);
		            						c++;
		            					}
	            						c1++;
	            						
	            					}
	            					
	            				}else if(pathCount == 4) {
	            					int contourCount = aFrameNode.getChildCount();
	            					int bFrameCount = bOrientationNode.getChildCount();
	            					selectedVOIModel.insertNodeInto(bFrameNode, bOrientationNode, bFrameCount);
	            					int c=0;
	            					for(int i=0;i<contourCount;i++) {
	            						selectedVOIModel.insertNodeInto((VOIContourNode)((VOIContourNode)selectedVOIModel.getChild(aFrameNode,i)).clone(), bFrameNode, c);
	            						c++;
	            					}
	            					
	            					
	            				}else if(pathCount == 5) {
	            					selectedVOIModel.insertNodeInto(bFrameNode, bOrientationNode, 0);
	            					selectedVOIModel.insertNodeInto(bNode, bFrameNode, 0);
	            				}
	            			//}
	            			
	            			break;
	            		}
	            		
	            	}
		            
		            
	            
	            
	            }

	            
	            selectedVOIModel.reload();
	            
            }else {
    
            	pathCount = ((TreePath)selectedValues[0]).getPathCount();

            	VOIOrientationNode aOrientationNode,bOrientationNode;
            	VOIFrameNode aFrameNode,bFrameNode,bFrameNode2;
            	VOIContourNode aNode,bNode,bNode2;

            	aFrameNode = null;
	        	bFrameNode = null;
	        	aNode = null;
	        	bNode = null;
	        	
	        	
	        	aOrientationNode = (VOIOrientationNode)(((TreePath)selectedValues[0]).getPathComponent(2));
	        	
	        	if(pathCount == 3) {

	        	}else if(pathCount == 4) {
	        		aFrameNode = (VOIFrameNode)(((TreePath)selectedValues[0]).getPathComponent(3));
	        		bFrameNode = (VOIFrameNode)aFrameNode.clone();

	        	}else if(pathCount == 5) {
	        		aFrameNode = (VOIFrameNode)(((TreePath)selectedValues[0]).getPathComponent(3));
		        	aNode = (VOIContourNode)(((TreePath)selectedValues[0]).getPathComponent(4));
		        	bFrameNode = (VOIFrameNode)aFrameNode.clone();
		        	bNode = (VOIContourNode)aNode.clone();
	        	}
	        	
	        	
	        	
	
	        	bFrameNode2 = null;
	        	
	        	
	        	
	        	
            	
            	int childCount1 = foundGroupNode.getChildCount();
            	for(int k=0;k<childCount1;k++) {
            		int childCount3 = 0;
            		bOrientationNode = (VOIOrientationNode)selectedVOIModel.getChild(foundGroupNode, k);
            		if(aOrientationNode.getName().equals(bOrientationNode.getName())) {
            			int childCount2 = bOrientationNode.getChildCount();
            			if(childCount2 > 0) {
            				boolean found2 = false;
            				for(int m=0;m<childCount2;m++) {
            					bFrameNode2 = (VOIFrameNode)selectedVOIModel.getChild(bOrientationNode, m);
            					if(bFrameNode2.getName().equals(aFrameNode.getName())) {
            						found2 = true;
            						childCount3 = bFrameNode2.getChildCount();
            						break;
            					}
            				}
            				if(found2) {
            					//int childCount3 = bFrameNode2.getChildCount();
            					boolean found3 = false;
            					for(int m=0;m<childCount3;m++) {
            						bNode2 = (VOIContourNode)selectedVOIModel.getChild(bFrameNode2, m);
            						if(bNode2.getName().equals(aNode.getName())) {
                						found3 = true;
                						break;
                					}
            					}
            					if(!found3) {
            						selectedVOIModel.insertNodeInto(bNode, bFrameNode2, childCount3);
            					}
            					
            				}else {
            					if(pathCount == 4) {
                					int contourCount = aFrameNode.getChildCount();
                					int bFrameCount = bOrientationNode.getChildCount();
                					selectedVOIModel.insertNodeInto(bFrameNode, bOrientationNode, bFrameCount);
                					int c=0;
                					for(int i=0;i<contourCount;i++) {
                						selectedVOIModel.insertNodeInto((VOIContourNode)((VOIContourNode)selectedVOIModel.getChild(aFrameNode,i)).clone(), bFrameNode, c);
                						c++;
                					}
                					
                					
                				}else if(pathCount == 5) {
                					selectedVOIModel.insertNodeInto(bFrameNode, bOrientationNode, 0);
                					selectedVOIModel.insertNodeInto(bNode, bFrameNode, 0);
                				}
            				}
            				
            				
            			}else {
            				if(pathCount == 3) {
            					int frameCount = aOrientationNode.getChildCount();
            					int c1=0;
            					for(int m=0;m<frameCount;m++) {
            						aFrameNode = (VOIFrameNode)selectedVOIModel.getChild(aOrientationNode, m);
            						bFrameNode = (VOIFrameNode)aFrameNode.clone();
            						
            						selectedVOIModel.insertNodeInto(bFrameNode, bOrientationNode, c1);
            						
            						int contourCount = aFrameNode.getChildCount();
            						
            						int c=0;
	            					for(int i=0;i<contourCount;i++) {
	            						
	            						selectedVOIModel.insertNodeInto((VOIContourNode)((VOIContourNode)selectedVOIModel.getChild(aFrameNode,i)).clone(), bFrameNode, c);
	            						c++;
	            					}
            						c1++;
            						
            					}
            					
            				}else if(pathCount == 4) {
            					int contourCount = aFrameNode.getChildCount();
            					selectedVOIModel.insertNodeInto(bFrameNode, bOrientationNode, contourCount);
            					int c=0;
            					for(int i=0;i<contourCount;i++) {
            						selectedVOIModel.insertNodeInto((VOIContourNode)((VOIContourNode)selectedVOIModel.getChild(aFrameNode,i)).clone(), bFrameNode, c);
            						c++;
            					}
            					
            					
            				}else if(pathCount == 5) {
            					selectedVOIModel.insertNodeInto(bFrameNode, bOrientationNode, 0);
            					selectedVOIModel.insertNodeInto(bNode, bFrameNode, 0);
            				}
            			}
            			
            			break;
            		}
            		
            	}
            	
            	
            	 selectedVOIModel.reload();
            	
            	
            }
            
           
            a.clearSelection();
        }
        
        
        protected void deleteFrom(JTree tree) {

            // ignore if there are no selections made.
            if (tree == null) {
                return;
            }

           
           
           TreePath[] selectedValues = tree.getSelectionModel().getSelectionPaths();
           
           if(selectedValues == null || selectedValues[0] == null) {
        	   return;
           }
           
           

           int pathCount = ((TreePath)selectedValues[0]).getPathCount();
           
           if(pathCount == 2) {
        	   VOIGroupNode bGroupNode = (VOIGroupNode)(((TreePath)selectedValues[0]).getPathComponent(1));
        	   selectedVOIModel.removeNodeFromParent(bGroupNode);
        	   index--;
        	   selectedVOIModel.reload();
        	   
        	   
           }else if(pathCount == 4) {
        	   VOIGroupNode bGroupNode = (VOIGroupNode)(((TreePath)selectedValues[0]).getPathComponent(1));
        	   VOIFrameNode bFrameNode = (VOIFrameNode)(((TreePath)selectedValues[0]).getPathComponent(3));
        	   selectedVOIModel.removeNodeFromParent(bFrameNode);
        	   selectedVOIModel.reload();
        	   
        	   //remove entire VOI if there are no more contours
        	   boolean deleteEntireVOI = true;
        	   int childCountFromGroupNode = selectedVOIModel.getChildCount(bGroupNode);
	            for(int k=0;k<childCountFromGroupNode;k++) {
	            	VOIOrientationNode orientationNode = (VOIOrientationNode)selectedVOIModel.getChild(bGroupNode, k);
	            	int childCountFromOrientationNode = selectedVOIModel.getChildCount(orientationNode);
	            	if(childCountFromOrientationNode > 0) {
	            		deleteEntireVOI = false;
	            		break;
	            	}
	            }
	            if(deleteEntireVOI) {
	            	 selectedVOIModel.removeNodeFromParent(bGroupNode);
	          	   index--;
	          	   selectedVOIModel.reload();
	            }
        	   
        	   
        	   
           }else if(pathCount == 5) {
        	   VOIGroupNode bGroupNode = (VOIGroupNode)(((TreePath)selectedValues[0]).getPathComponent(1));
        	   VOIContourNode bNode = (VOIContourNode)(((TreePath)selectedValues[0]).getPathComponent(4));
        	   selectedVOIModel.removeNodeFromParent(bNode);
        	   selectedVOIModel.reload();
        	   
        	   
        	   VOIFrameNode bFrameNode = (VOIFrameNode)(((TreePath)selectedValues[0]).getPathComponent(3));
        	   if(bFrameNode.getChildCount() == 0) {
        		   selectedVOIModel.removeNodeFromParent(bFrameNode);
            	   selectedVOIModel.reload();
        	   }
        	   
        	   
        	   
        	 //remove entire VOI if there are no more contours
        	   boolean deleteEntireVOI = true;
        	   int childCountFromGroupNode = selectedVOIModel.getChildCount(bGroupNode);
	            for(int k=0;k<childCountFromGroupNode;k++) {
	            	VOIOrientationNode orientationNode = (VOIOrientationNode)selectedVOIModel.getChild(bGroupNode, k);
	            	int childCountFromOrientationNode = selectedVOIModel.getChildCount(orientationNode);
	            	if(childCountFromOrientationNode > 0) {
	            		deleteEntireVOI = false;
	            		break;
	            	}
	            }
	            if(deleteEntireVOI) {
	            	 selectedVOIModel.removeNodeFromParent(bGroupNode);
	          	   index--;
	          	   selectedVOIModel.reload();
	            }
        	   
           }
           
        }

        
        
        
        private void copyAllToRight() {
        	//first delete all from right
        	
        	DefaultMutableTreeNode root = (DefaultMutableTreeNode)selectedVOIModel.getRoot();
             
             int childCountFromRoot = selectedVOIModel.getChildCount(root);

             if(childCountFromRoot > 0) {
	             for(int k=childCountFromRoot-1;k>=0;k--) {
	             	VOIGroupNode groupNode = (VOIGroupNode)selectedVOIModel.getChild(root, k);
	             	selectedVOIModel.removeNodeFromParent(groupNode);
	             	
	             }
             }
        	

        	
            Object sourceRoot =  sourceVoiModel.getRoot();
        	
        	int childCountFromRoot2 = sourceVoiModel.getChildCount(sourceRoot);

        	for(int m=0;m<childCountFromRoot2;m++) {
        		VOIGroupNode groupNode = (VOIGroupNode)sourceVoiModel.getChild(sourceRoot, m);
        		VOIGroupNode groupNode2 = new VOIGroupNode(groupNode.getVOIgroup(),image.getExtents());
        		
        		
        		selectedVOIModel.insertNodeInto(groupNode2, root, m);
        		
        	}
        	
        	selectedVOIModel.reload();
        
        }
        

        /**
         * Checks if all super's action commands are used, and ensures that the delete button removes items from listB,
         * and that duplicate items in listB are not repeated.
         * 
         * @param ae DOCUMENT ME!
         */
        public void actionPerformed(final ActionEvent ae) {

            // the buttons won't work getting lists if either are null.
            if ( (getRightTree() == null) || (getLeftTree() == null)) {
                return;
            }

            final String command = ae.getActionCommand();
            // check on other actions.
            if(!command.equals(ALLTREEB)) {
            	super.actionPerformed(ae);
            }

            

            if (command.equalsIgnoreCase("delete")) {
                deleteFrom(selectedVoiTree);
            }else if (command.equals(ALLTREEB)) {

            	copyAllToRight();
            }

            // prevent duplicate selections in listB:
           /* if (command.equalsIgnoreCase(TREEB)) {
                removeRepeatedElements(listB);
            }*/
        }
    }
    
    
    
    
    

    public class JPanelLogicalOptions extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -4726615904924397356L;

        

        /**
         * Creates a default layout, of the radio button options laid-out vertically. Currently, &quot;XML&quot; is not
         * selectable.
         */
        public JPanelLogicalOptions() {
            final ButtonGroup group = new ButtonGroup();
            setBorder(buildTitledBorder("Logical Operations"));
            setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
            and = new JRadioButton("AND", true);
            and.setFont(MipavUtil.font12);
            or = new JRadioButton("OR", false);
            or.setFont(MipavUtil.font12);
            xor = new JRadioButton("XOR", false);
            xor.setFont(MipavUtil.font12);


            // add to grouping
            group.add(and);
            group.add(or);
            group.add(xor);

            add(and);
            add(or);
            add(xor);
        }


    }
    
    
    
    public class JPanelImageOptions extends JPanel {
    	
    	  public JPanelImageOptions() {
    		  final ButtonGroup group = new ButtonGroup();
              setBorder(buildTitledBorder("Output Image"));
              setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
              createVoiImage = new JRadioButton("VOI Image", true);
              createVoiImage.setFont(MipavUtil.font12);
              createMaskImage = new JRadioButton("Mask Image", false);
              createMaskImage.setFont(MipavUtil.font12);
              
              group.add(createVoiImage);
              group.add(createMaskImage);

              add(createVoiImage);
              add(createMaskImage);
    	  }
    	
    }
    
    
    
    /**
     * An active listener for VOIEvents and ListSelectionEvents, this class will ensure the selected state of the VOI
     * and its name in a dialog's list is the same. It provides that when the name of the VOI is selected in a dialog's
     * list, the VOI itself is selected inside the image. Likewise, a selected VOI will highlight the name in the list
     * of the appropriate dialog.
     */
    public class VOIHighlighter implements ListSelectionListener, VOIListener {

        /**
         * We are not interested in adding Curves, so this method is empty.
         * 
         * @param added DOCUMENT ME!
         */
        public void addedCurve(final VOIEvent added) {
        /* not interested in adding curves */
        }

        /**
         * We are not interested in removing Curves, so this method is empty.
         * 
         * @param added DOCUMENT ME!
         */
        public void removedCurve(final VOIEvent added) {
        /* not interested in removing curves */
        }

        public void colorChanged(final Color c) {
        /* not interested in color change */
        }

        /**
         * Handles the VOI being selected. -- a state-change.
         * 
         * @param selection DOCUMENT ME!
         */
        public void selectedVOI(final VOIEvent selection) {
            //System.err.println( "VOIHighlighter.selectedVOI " + selection.getSource() + " " + selection.getState());
            //volumesList.setSelectedValue(selection.getSource(), selection.getState());
        }

        /**
         * Goes through the list selection event's JList source to find selected VOI list items. The selected VOIs are
         * then instructed to be &quot;active&quot;. Any other VOIs in the list are set to be not active.
         * 
         * @param lse DOCUMENT ME!
         */
        public void valueChanged(final ListSelectionEvent lse) {
            final JList imageVOIlist = (JList) lse.getSource();

            // go through all VOIs in the list. if the item is
            // selected, highlight the corresponding VOI,
            // otherwise, deselect it.
            // System.out.println("VOIHighlighter active");
            for (int i = 0; i < imageVOIlist.getModel().getSize(); i++) {

                if (imageVOIlist.isSelectedIndex(i)) {
                    ((VOI) imageVOIlist.getModel().getElementAt(i)).setAllActive(true);
                } else {
                    ((VOI) imageVOIlist.getModel().getElementAt(i)).setAllActive(false);
                }
            }

            // userInterface.getActiveImageFrame().repaint();
            if ( userInterface.getActiveImageFrame() != null ) {
                userInterface.getActiveImageFrame().updateImages();
            }
        }
    }



	/**
	 * 
	 */
	public ParameterTable createInputParameters() {
		final ParameterTable table = new ParameterTable();
		
		try {
		 table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
		}catch(Exception e) {
			
		}
		
		return table;
	}

	/**
	 * 
	 */
	public ParameterTable createOutputParameters() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * 
	 */
	public ActionMetadata getActionMetadata() {
		return new MipavActionMetadata() {
            public String getCategory() {
                return new String("VOI.VOI Logical Operations");
            }

            public String getDescription() {
                return new String("VOI Logical Operations");
            }

            public String getDescriptionLong() {
                return new String("VOI Logical Operations");
            }

            public String getShortLabel() {
                return new String("VOI Logical Operations");
            }

            public String getLabel() {
                return new String("VOI Logical Operations");
            }

            public String getName() {
                return new String("VOI Logical Operations");
            }
        };

	}

	/**
	 * 
	 */
	public String getOutputImageName(String imageParamName) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * 
	 */
	public boolean isActionComplete() {
		// TODO Auto-generated method stub
		return false;
	}


	public void setDoVOIImage(boolean doVOIImage) {
		this.doVOIImage = doVOIImage;
	}

	public void setLogicalOperation(int logicalOperation) {
		this.logicalOperation = logicalOperation;
	}
	
	
	
	



	
	

}
