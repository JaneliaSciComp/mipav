package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmVOILogicalOperations;
import gov.nih.mipav.model.algorithms.AlgorithmVOIProps;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIStatisticList;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.model.structures.event.VOIEvent;
import gov.nih.mipav.model.structures.event.VOIListener;
import gov.nih.mipav.model.structures.event.VOIVectorEvent;
import gov.nih.mipav.model.structures.event.VOIVectorListener;
import gov.nih.mipav.view.JPanelFileSelection;
import gov.nih.mipav.view.JPanelListController;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics.JPanelAddRemoveVOI;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics.JPanelStatisticFileFormatOptions;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics.VOIHighlighter;



/**
 * @author pandyan
 * 
 * This dialog and utility allows for logical operations of VOIs
 *
 */
public class JDialogVOILogicalOperations extends JDialogScriptableBase implements AlgorithmInterface, VOIStatisticList,
VOIVectorListener {
	
	
	 /** image and cloned image */
    protected ModelImage image, clonedImage;
    
    protected ViewUserInterface userInterface;
    
    /** Tabbed pane that holds all components. */
    protected JTabbedPane everything;
    
    /** Operator to provide listener access... could be done by /this/ */
    private VOIHighlighter highlighter;
    
    /** List of available VOIs. */
    private final JList volumesList = new JList();
    
    /** List of selected VOIs. */
    protected JList selectedList = new JList();
    
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
	
	
	
	
	/**
	 * constructor
	 * @param voiList
	 */
	public JDialogVOILogicalOperations(final VOIVector voiList) {
        super(ViewUserInterface.getReference().getMainFrame(), false);

        image = ViewUserInterface.getReference().getActiveImageFrame().getActiveImage();
        
        
        
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
        final Vector volumesVector = new Vector();

        for (int i = 0; i < voiList.size(); i++) {

            if (voiList.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                volumesVector.add(voiList.elementAt(i));
            }
        }

        volumesList.setListData(volumesVector);
        voiEvent.getVOI().addVOIListener(highlighter);
    }
    
    
    
    /**
     * resets the volumes list to the current VOIVector. removes the highlighter from the removed VOI.
     * 
     * @param voiEvent DOCUMENT ME!
     */
    public void removedVOI(final VOIVectorEvent voiEvent) {
        final VOIVector voiList = (VOIVector) voiEvent.getSource();
        final Vector volumesVector = new Vector();

        for (int i = 0; i < voiList.size(); i++) {

            if (voiList.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                volumesVector.add(voiList.elementAt(i));
            }
        }

        volumesList.setListData(volumesVector);

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
            selectedList.setSelectedValue(voiEvent.getVOI(), true);
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
        final Vector volumesVector = new Vector();
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
        volumesList.addListSelectionListener(highlighter);

        final JScrollPane jsp = new JScrollPane(volumesList);
        srctreep.add(jsp, BorderLayout.CENTER);

        // now let's listen to this vector:
        //VOIlist.addVectorListener(this);

        return srctreep;
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

        // this list to hold things so that they may be selectable/removable
        // panel to hold list access.
        selectedList.setListData(new Vector()); // = new JList();
        selp.add(new JScrollPane(selectedList), BorderLayout.CENTER);

        // build default arrowpanel
        if (addremove == null) {
            addremove = new JPanelAddRemoveVOI();
            addremove.setLeftList(volumesList);
            addremove.setRightList(selectedList);

            volumesList.addListSelectionListener(addremove);
            selectedList.addListSelectionListener(addremove);

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
		ViewVOIVector processList = new ViewVOIVector(selectedList.getModel().getSize());
        for(int i=0; i<selectedList.getModel().getSize(); i++) {
            processList.add((VOI)selectedList.getModel().getElementAt(i));
        }
        
        int logicalOperation = 0;
        
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
		// TODO Auto-generated method stub

	}

	/**
	 * 
	 */
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub

	}

	/**
	 * 
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(doVOIImage) {
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
			callAlgorithm();
		}else if(command.equals("Cancel")) {
			if(clonedImage != null) {
				 clonedImage.disposeLocal();
				 clonedImage = null;
			 }
			cleanUpAndDispose();
		}

	}
	
	
	 public void windowClosing(WindowEvent event) {
		 System.out.println("window closing");
		 if(clonedImage != null) {
			 clonedImage.disposeLocal();
			 clonedImage = null;
		 }
		 cleanUpAndDispose();
		 
	 }
	
	
	
	
	
	
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////    INNER CLASSES    ///////////////////////////////
	
	
	
	
	
	/**
     * controllas the lists between left and right side.
     */
    public class JPanelAddRemoveVOI extends JPanelListController {

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

        /**
         * Checks if all super's action commands are used, and ensures that the delete button removes items from listB,
         * and that duplicate items in listB are not repeated.
         * 
         * @param ae DOCUMENT ME!
         */
        public void actionPerformed(final ActionEvent ae) {

            // the buttons won't work getting lists if either are null.
            if ( (getRightList() == null) || (getLeftList() == null)) {
                return;
            }

            // check on other actions.
            super.actionPerformed(ae);

            final String command = ae.getActionCommand();

            if (command.equalsIgnoreCase("delete")) {
                deleteFrom(listB);
            }

            // prevent duplicate selections in listB:
            if (command.equalsIgnoreCase("listB")) {
                removeRepeatedElements(listB);
            }
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
            volumesList.setSelectedValue(selection.getSource(), selection.getState());
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

}
