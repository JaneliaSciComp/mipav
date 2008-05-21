import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.ScriptRecorder;
import gov.nih.mipav.model.scripting.actions.ActionCloseFrame;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.event.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogWinLevel;

import com.lowagie.text.*;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.pdf.*;

import java.awt.*;
import java.awt.Rectangle;
import java.awt.event.*;
import java.io.*;
import java.text.DecimalFormat;
import java.util.*;

import javax.swing.*;

public class PlugInMuscleImageDisplay extends ViewJFrameImage implements KeyListener {
    
    //~ Static fields --------------------------------------------------------------------------------------------------
    
    public static final Color[] colorPick = {Color.GREEN, Color.ORANGE, Color.CYAN, 
                                                Color.YELLOW, Color.MAGENTA};
    
    //~ Instance fields ------------------------------------------------------------------------------------------------    
    

    /**For writing PDF docs below. */
    private Document pdfDocument = null;
	private PdfWriter pdfWriter = null;
	private File pdfFile = null;
	private PdfPTable aTable = null;
	private PdfPTable imageTable = null;
    
    public static final String VOI_DIR = "NIA_Seg";
        
    /** Location of the VOI tab. */
    private int voiTabLoc;
    
    /** Location of the analysis tab. */
    private int resultTabLoc;
    
    /** Text for muscles where a mirror muscle may exist. */
    private String[][] mirrorArr;

    private boolean[][] mirrorZ;

    /** Text for muscles where mirror muscles are not considered. */
    private String[][] noMirrorArr;

    private boolean[][] noMirrorZ;

    /** 
     * Denotes the anatomical part represented in the image. Implemented seperatly in case 
     * this class is moved to its own class at a later time.
     */
    private ImageType imageType;

    /** Whether this image has mirror image muscles (eg VIEWS of thighs, abdomen. */
    private Symmetry symmetry;
    
    private JTabbedPane imagePane;
    
    private int activeTab;
    
    private DialogPrompt[] tabs;
    
    //private TreeMap zeroStatus;
    
    private boolean changeSlice = false;
    
    /**Whether the algorithm is dealing with a 3D CT image. */
    private boolean multipleSlices;
    
    private String[] titles; 
    
    private String imageDir;
    
    private boolean displayChanged = false;

    private int colorChoice = 0;
    
    private TreeMap<String, PlugInSelectableVOI> voiMap;
    
    private PlugInSelectableVOI[][] voiList;
    
    private TreeMap<String, Boolean> calcTree;
        
    public enum ImageType{
        
        /** denotes that the srcImg is an abdomen */
        Abdomen,
        
        /** denotes that the srcImg is two thighs */
        Thigh,
        
        /** unknown image type, generally represents an error state */
        Unknown
    }
    
    public enum Symmetry{
        
        /** Indicates the image has no symmetry. */
        NO_SYMMETRY, 
        
        /** Indicates that image has left-right symmetry. */
        LEFT_RIGHT, 
        
        /** Indicates the image has top-bottom symmetry. */
        TOP_BOTTOM
    }
    
    private boolean oldPrefCloseFrameCheckValue = Preferences.is(Preferences.PREF_CLOSE_FRAME_CHECK);
   
    public PlugInMuscleImageDisplay(ModelImage image, String[] titles,
            PlugInSelectableVOI[][] voiList,  
            ImageType imageType, Symmetry symmetry, boolean multipleSlices) {
    	//calls the super that will invoke ViewJFrameImage's init() function
    	super(image);
    	
    	this.setVisible(true);
        
        Preferences.setProperty(Preferences.PREF_CLOSE_FRAME_CHECK, String.valueOf(true));
        
        this.setImageA(image);
        this.setActiveImage(IMAGE_A);
        this.titles = titles;
        this.mirrorArr = new String[voiList.length][];
        this.mirrorZ = new boolean[voiList.length][];
        this.noMirrorArr = new String[voiList.length][];
        this.noMirrorZ = new boolean[voiList.length][];
        this.calcTree = new TreeMap();
        this.voiMap = new TreeMap();
        this.voiList = voiList;
        this.imageType = imageType;
        this.symmetry = symmetry;
        this.multipleSlices = multipleSlices;
        
        for(int i=0; i<voiList.length; i++) {
        	ArrayList mirrorArrList = new ArrayList(), noMirrorArrList = new ArrayList(), 
        				mirrorZList = new ArrayList(), noMirrorZList = new ArrayList();
        	for(int j=0; j<voiList[i].length; j++) {
        		voiMap.put(voiList[i][j].getName(), voiList[i][j]);
        		if(voiList[i][j].getName().contains("Left")) {
        			mirrorArrList.add(voiList[i][j].getName().substring(new String("Left ").length()));
        			mirrorZList.add(voiList[i][j].isFillable());
        			calcTree.put(voiList[i][j].getName().substring(new String("Left ").length()), voiList[i][j].doCalc());
        		} else if(voiList[i][j].getName().contains("Right")) {
        			//do nothing
        		} else {
        			noMirrorArrList.add(voiList[i][j].getName());
        			noMirrorZList.add(voiList[i][j].isFillable());
        			calcTree.put(voiList[i][j].getName(), voiList[i][j].doCalc());
        		}
        	}
        	mirrorArr[i] = new String[mirrorArrList.size()];
        	mirrorZ[i] = new boolean[mirrorZList.size()];
        	for(int j=0; j<mirrorArr[i].length; j++) {
        		mirrorArr[i][j] = (String)mirrorArrList.get(j);
        		mirrorZ[i][j] = (Boolean)mirrorZList.get(j);
        	}
        	noMirrorArr[i] = new String[noMirrorArrList.size()];
        	noMirrorZ[i] = new boolean[noMirrorZList.size()];
        	for(int j=0; j<noMirrorArr[i].length; j++) {
        		noMirrorArr[i][j] = (String)noMirrorArrList.get(j);
        		noMirrorZ[i][j] = (Boolean)noMirrorZList.get(j);
        	}
        }
        
        if (imageA == null) {
            return;
        }
        
        imageDir = getImageA().getFileInfo(getViewableSlice()).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
        
        initNext();
    }
    
    /**
     * Constructor for calling this as a standalone.  will invoke the class's own init() function
     * for setting up the image rather than ViewJFrameImage's init()
     * @param image the model image
     * @param titles
     * @param mirrorArr
     * @param mirrorZ
     * @param noMirrorArr
     * @param noMirrorZ
     * @param imageType
     * @param symmetry
     * @param standAlone
     */
    public PlugInMuscleImageDisplay(ModelImage image, String[] titles,
            PlugInSelectableVOI[][] voiList, 
            ImageType imageType, Symmetry symmetry, 
            boolean standAlone, boolean multipleSlices) {
    	// calls the super that will not call ViewJFrameImage's init() function
    	super(image, null, null, false, false);
    	this.setImageA(image);
        
        this.titles = titles;
        this.mirrorArr = new String[voiList.length][];
        this.mirrorZ = new boolean[voiList.length][];
        this.noMirrorArr = new String[voiList.length][];
        this.noMirrorZ = new boolean[voiList.length][];
        this.calcTree = new TreeMap();
        this.voiMap = new TreeMap();
        
        for(int i=0; i<voiList.length; i++) {
        	ArrayList mirrorArrList = new ArrayList(), noMirrorArrList = new ArrayList(), 
        				mirrorZList = new ArrayList(), noMirrorZList = new ArrayList();
        	for(int j=0; j<voiList[i].length; j++) {
        		voiMap.put(voiList[i][j].getName(), voiList[i][j]);
        		if(voiList[i][j].getName().contains("Left")) {
        			mirrorArrList.add(voiList[i][j].getName().substring(new String("Left ").length()));
        			mirrorZList.add(voiList[i][j].isFillable());
        			calcTree.put(voiList[i][j].getName().substring(new String("Left ").length()), voiList[i][j].doCalc());
        		} else if(voiList[i][j].getName().contains("Right")) {
        			//do nothing
        		} else {
        			noMirrorArrList.add(voiList[i][j].getName());
        			noMirrorZList.add(voiList[i][j].isFillable());
        			calcTree.put(voiList[i][j].getName(), voiList[i][j].doCalc());
        		}
        	}
        	mirrorArr[i] = new String[mirrorArrList.size()];
        	mirrorZ[i] = new boolean[mirrorZList.size()];
        	for(int j=0; j<mirrorArr.length; j++) {
        		mirrorArr[i][j] = (String)mirrorArrList.get(j);
        		mirrorZ[i][j] = (Boolean)mirrorZList.get(j);
        	}
        	noMirrorArr[i] = new String[noMirrorArrList.size()];
        	noMirrorZ[i] = new boolean[noMirrorZList.size()];
        	for(int j=0; j<noMirrorArr.length; j++) {
        		noMirrorArr[i][j] = (String)noMirrorArrList.get(j);
        		noMirrorZ[i][j] = (Boolean)noMirrorZList.get(j);
        	}
        }
        this.imageType = imageType;
        this.symmetry = symmetry;
        this.multipleSlices = multipleSlices;
        
        Preferences.setProperty(Preferences.PREF_CLOSE_FRAME_CHECK, String.valueOf(true));
                
    	initStandAlone();
    	this.setActiveImage(IMAGE_A);
    	setVisible(true);
    	scrollPane.requestFocus();
    	
    	imageDir = getImageA().getFileInfo(getViewableSlice()).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
    }
    
    /**
     * Initializes the frame and variables.
     *
     */
    private void initStandAlone() throws OutOfMemoryError {
        initResolutions();
        initZoom();
        initLUT();

        int[] extents = createBuffers();

        initComponentImage(extents);
        initExtentsVariables(imageA);

        // create and build the menus and controls
        controls = new ViewControlsImage(this); // Build controls used in this frame
        menuBuilder = new ViewMenuBuilder(this);

        // build the menuBar based on the number of dimensions for imageA
        menuBarMaker = new ViewMenuBar(menuBuilder);

        //add pre-defined UIParams to the vector
        Vector<CustomUIBuilder.UIParams> voiParams = new Vector<CustomUIBuilder.UIParams>();
        voiParams.addElement(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER);
        voiParams.addElement(CustomUIBuilder.PARAM_VOI_LEVELSET);
        voiParams.addElement(CustomUIBuilder.PARAM_VOI_LIVEWIRE);
        voiParams.addElement(CustomUIBuilder.PARAM_VOI_ELLIPSE);
        voiParams.addElement(CustomUIBuilder.PARAM_VOI_RECTANGLE);
        voiParams.addElement(CustomUIBuilder.PARAM_PAINT_FILL);
        voiParams.addElement(CustomUIBuilder.PARAM_PAINT_ERASE_SLICE);
        
        Vector<CustomUIBuilder.UIParams> voiActionParams = new Vector<CustomUIBuilder.UIParams>();
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_COLOR);
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_PROPERTIES);
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_NEW);
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_UNDO);
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_CUT);
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_COPY);
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_PASTE);
       
        
        //build the toolbar and add the custom button vectors
        controls.buildSimpleToolBar();
        controls.addCustomToolBar(voiParams);
        controls.addCustomToolBar(voiActionParams);
                
        // MUST register frame to image models
        imageA.addImageDisplayListener(this);

        windowLevel = new JDialogWinLevel[2];      
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

        pack();
        
        // User interface will have list of frames
       userInterface.registerFrame(this);

       getContentPane().add(controls, BorderLayout.NORTH);
        
        //call the normal init function here
        initNext();
    } // end init()
    
    /**
     * Cleans memory.
     *
     * @throws  Throwable  the <code>Exception</code> raised by this method
     */
    public void finalize() throws Throwable {
    	
    	ViewUserInterface.getReference().unregisterFrame(this);
    	
    	if (imageA != null) {
    		imageA.disposeLocal(true);
    	}
    	if (componentImage != null) {
    		componentImage.disposeLocal(true);
    	}
    	removeComponentListener(this);
    	for (int i = 0; i < tabs.length; i++) {
    		tabs[i].removeAll();
    		tabs[i].removeComponentListener(this);
    		tabs[i] = null;
    	}
    	tabs = null;
    	
    	imagePane.removeAll();
    	imagePane = null;

    	System.gc();
    	
    	if (ViewUserInterface.getReference().isAppFrameVisible() == false &&
    			ViewUserInterface.getReference().getActiveImageFrame() == null) {
    		System.exit(0);
    	}
    	
    }
        
    /**
     * This method saves all VOIs for the active image to a given directory.  Note:  This method differs quite a bit in execution 
     * compared with saveALLVOIsTo(voiDir) in ViewJFrameBase.
     *
     * @param  voiDir  directory that contains VOIs for this image.
     */
    public void saveAllVOIsTo(String voiDir) {

        int nVOI;
        int i;
        ViewVOIVector VOIs;
        FileVOI fileVOI;
        ModelImage currentImage;

        try {

            if (displayMode == IMAGE_A) {
                currentImage = componentImage.getImageA();
                VOIs = currentImage.getVOIs();
            } else if (displayMode == IMAGE_B) {
                currentImage = componentImage.getImageB();
                VOIs =  currentImage.getVOIs();
            } else {
                MipavUtil.displayError(" Cannot save VOIs when viewing both images");

                return;
            }

            // Might want to bring up warning message before deleting VOIs !!!!
            // or not do it at all.
            // if voiDir exists, then empty it
            // if voiDir does not exist, then create it
            File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) {

                // only clean out the vois if this is a default voi directory
                if (voiFileDir.getName().startsWith("defaultVOIs_")) {
                    File[] files = voiFileDir.listFiles();

                    if (files != null) {

                        for (int k = 0; k < files.length; k++) {

                            if (files[k].getName().endsWith(".voi") || files[k].getName().endsWith(".xml")) { // files[k].delete();
                            }
                        }
                    } // if (files != null)
                }
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
            } else { // voiFileDir does not exist
                voiFileDir.mkdir();
            }

            nVOI = VOIs.size();

            System.err.println("Number of VOIs: " + nVOI);

            for (i = 0; i < nVOI; i++) {

                fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, currentImage);

                fileVOI.writeVOI(VOIs.VOIAt(i), true);
            }

        } catch (IOException error) {
            MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
        }

    } // end saveAllVOIsTo()
    
    @Override
    public void actionPerformed(ActionEvent e) {
        displayChanged = false;
        String command = e.getActionCommand();
        
        //run through toggle buttons to see if a menu selected one (updates the button status)
        getControls().getTools().setToggleButtonSelected(command);
                
        if(command.equals(DialogPrompt.CHECK_VOI)) {
        	String name;
            ((VoiDialogPrompt)tabs[voiTabLoc]).setUpDialog(name = ((JButton)(e.getSource())).getText(), 
            		true, voiMap.get(name).getNumCurves());
            lockToPanel(voiTabLoc, "VOI"); //includes making visible
            //TODO: add here
            initVoiImage(activeTab); //replacing current image and updating
	    	displayChanged = true;
        } else if(command.equals(DialogPrompt.CALCULATE)) {
        	lockToPanel(resultTabLoc, "Analysis"); //includes making visible
        	getActiveImage().unregisterAllVOIs();
	    	updateImages(true);
	    	displayChanged = true;
        	((AnalysisPrompt)tabs[resultTabLoc]).setButtons();
        	((AnalysisPrompt)tabs[resultTabLoc]).performCalculations();
        } else if (!(command.equals(DialogPrompt.OUTPUT) ||
        		command.equals(DialogPrompt.SAVE) ||
        		command.equals(DialogPrompt.OUTPUT_ALL))) {
        	if(command.equals(DialogPrompt.CANCEL)) {
        		unlockToPanel(voiTabLoc);
        		initMuscleImage(activeTab);
        	} else if(command.equals(DialogPrompt.HIDE_ALL)) {
        		getActiveImage().unregisterAllVOIs();
        		updateImages(true);
        	} else if(command.equals(DialogPrompt.OK) && 
        			tabs[voiTabLoc].completed() == true) {
        		unlockToPanel(voiTabLoc);
        		System.out.println("Place to look at voi based on activeTab: "+activeTab);
        		initMuscleImage(activeTab);
        	} else if(command.equals(DialogPrompt.BACK)) {
        		unlockToPanel(resultTabLoc);
        		initMuscleImage(activeTab);
        	} else if(command.equals(DialogPrompt.EXIT)) {
            	close();
        	} else if(command.equals(DialogPrompt.HELP)) {
        		PlugInMuscleSegmentation.showHelp("MS00010"); //20, and 30 also available
        	} else {
        		super.actionPerformed(e);
        	}
        } else {
        	super.actionPerformed(e);
        }
    }
    
    /**
     * Closes window and disposes of the PlugInMuscleImageDisplay frame.  From ViewJFrameImage since the super method was
     * throwing the program into a loop since the original image and the PlugInMuscleImageDisplay are tethered but only one should be closed.
     */
    public void close() {

        if (Preferences.is(Preferences.PREF_CLOSE_FRAME_CHECK)) {
            int reply = JOptionPane.showConfirmDialog(this, "Do you really want to close this plugin?", "Close Frame",
                                                      JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
            if (reply == JOptionPane.NO_OPTION) {
                return;
            }
        }

        ScriptRecorder.getReference().addLine(new ActionCloseFrame(getActiveImage()));
        ProvenanceRecorder.getReference().addLine(new ActionCloseFrame(getActiveImage()));

        setVisible(false);
        try {
            this.finalize();
        } catch (Throwable t) {
            MipavUtil.displayError("Error encountered cleaning up image frame: " + t);
        }
        
        Preferences.setProperty(Preferences.PREF_CLOSE_FRAME_CHECK, String.valueOf(oldPrefCloseFrameCheckValue));
        
        //next step would normally be to finalize, but we don't want the controls removed since the original image
        //may still be worked on

        System.gc();
    }
    
    private JPanel buildMainPanel() {
    	//The component image will be displayed in a scrollpane.       
        scrollPane = new JScrollPane(componentImage, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
        componentImage.useHighlight(false);
        scrollPane.setFocusable(true);
        scrollPane.setBackground(Color.black);
        scrollPane.addKeyListener(this);
        
        imagePane = new JTabbedPane();
        imagePane.setMinimumSize(new Dimension (370, 532));
        imagePane.setPreferredSize(new Dimension(370, 532));
        imagePane.setMaximumSize(new Dimension (370, 532));
        
        tabs = new DialogPrompt[mirrorArr.length+2]; //+2 for VOI and AnalysisPrompt
        
        JPanel panelA = new JPanel(new GridLayout(1, 3, 10, 10));
        
        JPanel testPanel = new JPanel();
        testPanel.setLayout(new BorderLayout());
        testPanel.add(imagePane, BorderLayout.WEST);
        testPanel.add(scrollPane, BorderLayout.CENTER);
        
        //JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, imagePane, scrollPane);
       
        panelA.add(testPanel);
        
        //panelA.add(new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, imagePane, scrollPane));
     
        for(int i=0; i<mirrorArr.length; i++) { 
        	tabs[i] = new MuscleDialogPrompt(this, titles[i], mirrorArr[i], 
                    noMirrorArr[i], 
                    imageType, symmetry, i);

        	tabs[i].addComponentListener(this);
        	tabs[i].addListener(this);
        	tabs[i].setName(titles[i]);
        	imagePane.addTab((i+1)+": "+titles[i], tabs[i]);
        	
        	JButton[] mirror = ((MuscleDialogPrompt)tabs[i]).getMirrorButton();
            JButton[] noMirror = ((MuscleDialogPrompt)tabs[i]).getNoMirrorButton();
        }
        //now put voiTab up
        voiTabLoc = mirrorArr.length;
        tabs[voiTabLoc] = new VoiDialogPrompt(this);
        tabs[voiTabLoc].addListener(this);
        tabs[voiTabLoc].addComponentListener(this);
        tabs[voiTabLoc].setVisible(false);
        
        //now put resultsTab up
        resultTabLoc = mirrorArr.length+1;
        tabs[resultTabLoc] = new AnalysisPrompt(this, mirrorArr, noMirrorArr);
        tabs[resultTabLoc].addListener(this);
        tabs[resultTabLoc].addComponentListener(this);
        tabs[resultTabLoc].setVisible(false);
                
        return panelA;
    }
    
    private void initNext() {
        
    	JPanel mainPanel = buildMainPanel();
    	
    	//if this is standalone (app frame hidden), add the tabbedpane from the messageframe to the bottom of the plugin's frame
    	if (ViewUserInterface.getReference().isAppFrameVisible()) {
    		getContentPane().add(mainPanel, BorderLayout.CENTER);
    	} else {
    		JTabbedPane messageTabs = ViewUserInterface.getReference().getMessageFrame().getTabbedPane();
    		messageTabs.setPreferredSize(new Dimension(this.getWidth(), 100));
    		JSplitPane mainSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT, mainPanel, messageTabs );
    		mainSplit.setDividerLocation(550);
    		getContentPane().add(mainSplit, BorderLayout.CENTER);
    	}
        
    	//removes extra scrollPane from the mipav-loaded plugin
    	if (ViewUserInterface.getReference().isAppFrameVisible()) {
        	getContentPane().remove(0);
        } 

        pack();
        initMuscleImage(0);
        if (ViewUserInterface.getReference().isAppFrameVisible()) {
        	this.setMinimumSize(new Dimension(380, 550));
        } else {
        	this.setMinimumSize(new Dimension(380, 640));
        }
        this.setResizable(true);
    }
    
    private boolean voiChangeState = false;
    
    @Override
	public void componentShown(ComponentEvent event) {
    	Component c = event.getComponent();
	    if(c instanceof MuscleDialogPrompt) {
	    	for(int i=0; i<voiTabLoc; i++) {
	    		if(tabs[i].equals(c) && !voiChangeState) {
	    			initMuscleImage(i);
	    			activeTab = i;
	    			displayChanged = true;
	    		}
	    	}
	    } 
	    super.componentShown(event);
	}

	/**
     * Version of componentResized that is very similar to super, but works with the embedded dialog box.
     *
     * @param  event  event that triggered function
     */
    public synchronized void componentResized(ComponentEvent event) {
    	int width, height, imageWidth, imageHeight;
        float bigger;
        int minFrameWidth = 123;  ///minimum frame width... function of java or windows?  need to check w\ linux build
        
        boolean imageSizeSmall = false;
        //check to see if the image width is SMALLER than the minimum frame width
        if (componentImage.getActiveImage().getExtents()[0] <= minFrameWidth) {
        	imageSizeSmall = true;     
        }
            
        // if the window size is greater than the display window size - 20 (in either direction)
        //  do nothing
        if ((getSize().width >= (xScreen - 20)) || (getSize().height >= (yScreen - 20))) {
            return;
        }

       

        //width and height calculated by size minus both insets of entire image+dialogbox
        width = getSize().width - (getInsets().left + getInsets().right);
        height = getSize().height - (getInsets().top + getInsets().bottom);
        
        width -= (scrollPane.getInsets().left + scrollPane.getInsets().right);
        height -= (scrollPane.getInsets().top + scrollPane.getInsets().bottom);
        
        //width and height of image only, for use in detecting whether zoom occurred
        imageWidth = componentImage.getSize(null).width - (getInsets().left + getInsets().right);
        imageHeight = componentImage.getSize(null).height - (getInsets().top + getInsets().bottom);
        
        imageWidth -= (scrollPane.getInsets().left + scrollPane.getInsets().right);
        imageHeight -= (scrollPane.getInsets().top + scrollPane.getInsets().bottom);
        
        //determine the larger of width/height
        //in order to find the zoom based the current window size (not necessarily the current zoom)
        bigger = Math.max(imageWidth, imageHeight);
        zoom = (int) Math.min((bigger ) / ((imageA.getExtents()[0] * widthResFactor) ),
                              (bigger ) / ((imageA.getExtents()[1] * heightResFactor) ));

        
        //check to see if we are dealing with a small sized image at the minimum frame width
        if (imageSizeSmall && (zoom > componentImage.getZoomX()) && (getSize().width <= minFrameWidth)) {
        	
        		//System.err.println("Doing nothing, returning\n\n\n");
        	return;
        }
        
        // remove the componentListener so this function will not be called twice
        removeComponentListener(this);
        
        //System.err.println("Calculated zoom: " + zoom);
        //System.err.println("ComponentImage size (pre-adjustment): " + componentImage.getSize(null));
        
        //if the zoom is larger than the current zoom, set the current zoom to the calculated zoom
        if (zoom > componentImage.getZoomX()) {
        	//System.err.println("Setting componentImage to calculated zoom");
            componentImage.setZoom((int) zoom, (int) zoom); // ***************************
           
            updateImages(true);

            //checking componentImage size after updateImages (with new zoom)
             
            if ((componentImage.getSize(null).width + 200) > xScreen) {
                width = xScreen - 200;
            } else {
                width = componentImage.getSize(null).width;
            }

            if ((componentImage.getSize(null).height + 200) > yScreen) {
                height = yScreen - 200;
            } else {
                height = componentImage.getSize(null).height;
            }
           // System.err.println("componentImage size now: " + componentImage.getSize(null));
            
        } else if ((imageWidth < componentImage.getSize(null).width) && (imageHeight >= componentImage.getSize(null).height)) {
        	//System.err.println("Width is less than compImage.width, height is greater than compImage.height");

            height = componentImage.getSize(null).height + scrollPane.getHorizontalScrollBar().getHeight();
        } else if ((imageWidth >= componentImage.getSize(null).width) && (imageHeight < componentImage.getSize(null).height)) {
            width = componentImage.getSize(null).width + scrollPane.getVerticalScrollBar().getWidth();

            //System.err.println("Height is less than compImage.height, width is greater than compImage.width");
                        
        } else if ((imageWidth < componentImage.getSize(null).width) || (imageHeight < componentImage.getSize(null).height)) { // width += fudgeFactor;
           
        	//System.err.println("either width is less than component width or height is less than component height... returning\n\n");
        	addComponentListener(this);

            return;
        } else if ((imageWidth > componentImage.getSize(null).width) || (imageHeight > componentImage.getSize(null).height)) {

        	//System.err.println("Width or height is greater than compImage width/height, setting to compImage width and height");
        	
            if (width > componentImage.getSize(null).width) {
                width = componentImage.getSize(null).width;
            }

            if (height > componentImage.getSize(null).height) {
                height = componentImage.getSize(null).height;
            }
        } else {
        	//System.err.println("apparently width and height are set okay (comparing to compeditimage)...returning\n\n");
        	
            addComponentListener(this);

            return;
        }

       // System.err.println("Adjusting scrollpane width, height with scrollpane insets: " + scrollPane.getInsets());
        //moves only based on zoom
        imageWidth += scrollPane.getInsets().left + scrollPane.getInsets().right;
        imageHeight += scrollPane.getInsets().top + scrollPane.getInsets().bottom;
        
     //   System.err.println("Old scrollpane width, height: " + scrollPane.getSize());
        
        
        
        if (scrollPane.getSize().width != imageWidth ||
        		scrollPane.getSize().height != imageHeight) {
        	
        	scrollPane.setSize(imageWidth, imageHeight);
        	
            setSize(width,
                    height);
            validate();
            setTitle();
           
            updateImages(true);
        }
        
        addComponentListener(this);
        
    }
    
    /**Loads relevant VOIs for the particular slice. */
    
    @Override
    public void setSlice(int slice, boolean updateLinkedImages) {
    	
    	if(slice != getViewableSlice())
    		changeSlice = true;
    	super.setSlice(slice, updateLinkedImages);
    	if(changeSlice && activeTab < voiTabLoc) {
    		if(tabs[resultTabLoc].isVisible()) {
    			((AnalysisPrompt)tabs[resultTabLoc]).setButtons();
    			getActiveImage().unregisterAllVOIs();
    		} else {
    			((MuscleDialogPrompt)tabs[activeTab]).clearButtons();
    			initMuscleImage(activeTab);
    		}
    		updateImages(true);
    		changeSlice = false;
    	}
    	
    }
    
    /**
     * private void computeIdealWindowSize() This method will enlarge or shrink the window size in response to the
     * componentImage being zoomed. It will only resize the window to IMAGE_SCREEN_RATIO of the screen size, in this
     * case 3/5. If the image is zoomed past that size, scrollbars are added. If the user has dragged the window to a
     * size larger than 3/5 of screen size, I assume he wants it that way and the window will not be resized in that
     * case upon zoom-in.
     */
    protected void computeIdealWindowSize() {
        boolean addInsets = true;
        int newWidth = getContentPane().getWidth();
        int newHeight = getContentPane().getHeight();
       // int newWidth = getScrollPaneSize().width;
       // int newHeight = getScrollPaneSize().height;

        final float IMAGE_SCREEN_RATIO = 3.0f / 5.0f; // image will not be resized past 3/5 of screen size

        // if the image is too wide, cap the new window width
        if (componentImage.getSize().width > xScreen * IMAGE_SCREEN_RATIO) {
            addInsets = false;
            newWidth = (int) (xScreen * IMAGE_SCREEN_RATIO);
        }

        // if the image is too tall, cap the new window height
        if (componentImage.getSize().height > yScreen * IMAGE_SCREEN_RATIO) {
            addInsets = false;
            newHeight = (int) (yScreen * IMAGE_SCREEN_RATIO);
        }

        // if the window is already wider than IMAGE_SCREEN_RATIO, do not
        // resize the window, since the only way it could have got that big
        // is if the user manually resized it to be that large
        if ((getSize().width > xScreen * IMAGE_SCREEN_RATIO) &&
                (componentImage.getSize().width > getSize().width)) {
            addInsets = false;
            newWidth = getSize().width;
        }

        // if the window is already taller than IMAGE_SCREEN_RATIO, do not
        // resize the window, since the only way it could have got that big
        // is if the user manually resized it to be that large
        if ((getSize().height > yScreen * IMAGE_SCREEN_RATIO) &&
                (componentImage.getSize().height > getSize().height)) {
            addInsets = false;
            newHeight = getSize().height;
        }

        scrollPane.setSize(newWidth, newHeight);

        if (addInsets == true) {
            setSize(getFrameWidth(), getFrameHeight());
        } else {
            setSize(newWidth, newHeight);
        }
    }
    
    /**
     * Lock the dialog to a specific panel, such as VOI or Analysis
     * 
     * @param tabLoc the tab to open
     * @param title the title of the tab to open
     */
    public void lockToPanel(int tabLoc, String title) {
    	tabs[tabLoc].setVisible(true);
    	for(int i=0; i<voiTabLoc; i++)
    		imagePane.setEnabledAt(i, false);
    	imagePane.addTab(title, tabs[tabLoc]);
    	imagePane.setSelectedIndex(voiTabLoc);
    	updateImages(true);
    }
    
    /**
     * Unlocks the dialog, allowing movement between tabs while closing the current process.
     * 
     * @param closingTabLoc the tab to remove
     */
    public void unlockToPanel(int closingTabLoc) {
    	tabs[closingTabLoc].setVisible(false);
    	imagePane.remove(tabs[closingTabLoc]);
        for(int i=0; i<voiTabLoc; i++) 
            imagePane.setEnabledAt(i, true);
        imagePane.setSelectedIndex(activeTab);
        voiChangeState = false;
    }
    
    /**
     * Identifies which panel a particular VOI belongs to
     * 
     * @param name the VOI
     * @return a panel where 0 is first
     */
    public int getLocationStatus(String name) {
    	int loc = PlugInSelectableVOI.INVALID_LOCATION;
    	if(voiMap.get(name) != null)
    		loc = voiMap.get(name).getLocation();
    	return loc;
    }
    
    /**
     * Identifies whether a VOI should be filled in
     * 
     * @param name the VOI
     * @return true, VOI to be filled
     */
    public boolean getZeroStatus(String name) {
    	boolean fill = false;
    	if(voiMap.get(name) != null)
    		fill = voiMap.get(name).isFillable();
    	return fill;
    }
    
    private void loadVOI(int pane) {
    	System.err.println("calling loadVOI");
        
    	//getImageA().unregisterAllVOIs();
        String fileDir;
    	if(multipleSlices)
    		fileDir = getImageA().getFileInfo(0).getFileDirectory()+VOI_DIR+"_"+getViewableSlice()+"\\";
    	else
    		fileDir = getImageA().getFileInfo(0).getFileDirectory()+VOI_DIR+"\\";
        File allVOIs = new File(fileDir);
        //ArrayList paneVOIs = new ArrayList();
        if(allVOIs.isDirectory()) {
            String[] voiName = allVOIs.list();
            for(int i=0; i<voiName.length; i++) {
            	//voiName[i] = voiName[i].substring(0, voiName[i].indexOf(".xml"));
            	//if(getLocationStatus(voiName[i]) == pane)
            	//	paneVOIs.add(voiName[i]);
            	
            	if(voiName[i].indexOf(".xml") != -1) {
	            	
	            	String name = voiName[i].substring(0, voiName[i].indexOf(".xml"));
	            	String ext = ".xml";
	            	VOI v;
	            	System.out.println(name);
	            	if(voiMap.get(name).getLocation() == pane) {
	            		v = getSingleVOI(name+ext);
	            		if(v != null) {
	            			v.setThickness(2);
	            			Color c = PlugInSelectableVOI.INVALID_COLOR;
	            			
	            			if((c = voiMap.get(v.getName()).getColor()).equals(PlugInSelectableVOI.INVALID_COLOR)) {
		            			//System.out.println("A new one: "+v.getColor());
		                    	if((c = hasColor(v)).equals(PlugInSelectableVOI.INVALID_COLOR))
		                            v.setColor(c = colorPick[colorChoice++ % colorPick.length]);
		                    	else
		                    		v.setColor(c);
		                    	voiMap.get(v.getName()).setColor(c);
	            			} else
	            				v.setColor(c);
	            			v.setDisplayMode(VOI.BOUNDARY);
	            			getActiveImage().registerVOI(v);
	            		}
	            	}
            	}
            	
            }
            //String[] nameList = new String[paneVOIs.size()];
            //for(int i=0; i<nameList.length; i++)
           // 	nameList[i] = (String)paneVOIs.get(i);
            //loadVOIs(nameList, false);
            updateImages(true);
        }
    }
    
    private void initMuscleImage(int pane) {        
    	if(!changeSlice) 
        	getImageA().unregisterAllVOIs();
    	
    	componentImage.setCursorMode(ViewJComponentBase.NEW_VOI);
    	loadVOI(pane);
        
        ctMode(getImageA(), -175, 275);
        
        VOIVector vec = getActiveImage().getVOIs();
        
        for(int i=0; i<vec.size(); i++) { 
        	((MuscleDialogPrompt)tabs[pane]).setButton(vec.get(i), vec.get(i).getName(), vec.get(i).getColor());
        }
        
    	this.repaint();

        updateImages(true);
    }
    
    private void initVoiImage(int pane) {
    	//getActiveImage().unregisterAllVOIs();
    	//load VOIs of activeTab
    	loadVOI(pane);
        VOIVector voiVec = getActiveImage().getVOIs();
    	for(int i=0; i<voiVec.size(); i++) {
    		VOI voi = voiVec.get(i);
    		if(voiMap.get(voi.getName()).isFillable() && 
    				!(((VoiDialogPrompt)tabs[voiTabLoc]).getObjectName().equals(voi.getName()))) 
    			voi.setDisplayMode(VOI.SOLID);
    	}
        
        componentImage.setCursorMode(ViewJComponentBase.NEW_VOI);
        updateImages(true);
    }

    /**
     * Sets mode to CT and sets range to CT presets.
     *
     * @param  preset1  first CT preset
     * @param  preset2  second CT preset
     */
    public void ctMode(ModelImage image, int preset1, int preset2) {
    	Dimension dim = new Dimension(256, 256);
    	
    	//stores LUT min max values
    	float[] x = new float[4], y = new float[4], z = new float[4];
    	
    	//reference to the image data currently displayed, used to adjust transfer func
    	float[] dataSlice;
    	
    	float min = Float.MAX_VALUE;
        float max = -Float.MIN_VALUE;
        //image's max and min intensities
        float minImage, maxImage;
        int i;
        
        ModelLUT LUT = getComponentImage().getLUTa();
      
        //Stores the maximum and minimum intensity values applicable to this image
        minImage = (float)image.getMin();
        maxImage = (float)image.getMax();
        
        dataSlice = getComponentImage().getActiveImageBuffer();
        min = Float.MAX_VALUE;
        max = -Float.MAX_VALUE;

        for (i = 0; i < dataSlice.length; i++) {

            if (dataSlice[i] > max) {
                max = dataSlice[i];
            }

            if (dataSlice[i] < min) {
                min = dataSlice[i];
            }
        }
        
        //Set LUT min max values of the image slice !!
        x[0] = minImage;
        y[0] = 255;
        z[0] = 0;
        x[1] = min;
        y[1] = 255;
        z[1] = 0;
        x[2] = max;
        y[2] = 0;
        z[2] = 0;
        x[3] = maxImage;
        y[3] = 0;
        z[3] = 0;
        LUT.getTransferFunction().importArrays(x, y, 4);
        
        float yVal, m, b;
        min = (float) image.getMin();
        max = (float) image.getMax();

        x[0] = min; // -1024;
        y[0] = dim.height - 1;
        z[0] = 0;

        if (preset2 < max) {
            x[2] = preset2;
        } else {
            x[2] = max;
        }

        y[2] = 0;
        z[2] = 0;

        if (preset1 < min) {

            // y = m * x + b, line equation
            // Assume given points: pt1 ( preset1, 255 ),  pt2 ( x[2], y[2])
            // find point: pt3 ( -1024, yVal);
            m = (255 - y[2]) / (preset1 - x[2]);
            b = 255 - (m * preset1);
            yVal = (m * (-1024)) + b;
            x[1] = -1024;
            y[1] = yVal;
            z[1] = 0;
            Preferences.debug("yVal = " + yVal);
        } else {
            x[1] = preset1;
            y[1] = dim.height - 1;
            z[1] = 0;
        }

        if (y[1] > 255) {
            y[1] = 255;
        }

        x[3] = max; // 3071;
        y[3] = 0;
        z[3] = 0;
        
        LUT.getTransferFunction().importArrays(x, y, 4);
        image.notifyImageDisplayListeners(LUT, true);
    }
    

    

    private abstract class DialogPrompt extends JPanel implements ActionListener {
    	
    	//~ Static fields/initializers -------------------------------------------------------------------------------------
    	
    	/**Action commands without associated buttons (usually for OK buton) */
    	public static final String CHECK_VOI = "Check VOI";
    	public static final String DIALOG_COMPLETED = "Dialog Completed";
    	
    	/** Possible buttons for dialog prompts. */
    	public static final String OK = "Ok";
    	public static final String CANCEL = "Cancel";
    	public static final String HIDE_ALL = "Hide all";
    	public static final String SHOW_ALL = "Show all";
    	public static final String EXIT = "Exit";
    	public static final String HELP = "Help";
    	public static final String CALCULATE = "Calculate";
    	public static final String SAVE = "Save";
    	public static final String OUTPUT_ALL = "Output All";
    	public static final String TOGGLE_LUT = "Toggle LUT";
    	public static final String OUTPUT = "Output";
    	public static final String BACK = "Back";
    	public static final String RESET = "Reset";
    	public static final String HIDE_ONE = "Hide except";
    	
    	private String buttonStringList[] = {OK, HIDE_ALL, HELP};
    	
    	protected JButton buttonGroup[];
    	
    	protected PlugInMuscleImageDisplay muscleFrame;
    	
    	private Vector<ActionListener> objectList = new Vector<ActionListener>();
    	
    	private String title;
    	
    	protected boolean completed = false;
    	
    	public DialogPrompt(PlugInMuscleImageDisplay theParentFrame, String title) {
    		this.muscleFrame = theParentFrame;
    		this.title = title;
    	}
    	
    	public DialogPrompt(PlugInMuscleImageDisplay theParentFrame, String title, String[] buttonString) {
    		this.muscleFrame = theParentFrame;
    		this.title = title;
    		this.buttonStringList = buttonString;
    	}
    	
    	public String getTitle() {
    		return title;
    	}
    	
    	public boolean completed() {
    		return completed;
    	}
    	
    	protected void setButtons(String[] buttonString) {
    		this.buttonStringList = buttonString;
    	}
    	
    	protected abstract void initDialog();
    	
    	/**
         * Builds button panel consisting of OK, Cancel and Help buttons.
         *
         * @return  JPanel that has ok, cancel, and help buttons
         */
        protected JPanel buildButtons() {
            JPanel buttonPanel = new JPanel();
            buttonGroup = new JButton[buttonStringList.length];
            
            if (buttonGroup.length > 3) {
            	JPanel topPanel = new JPanel();
            	JPanel bottomPanel = new JPanel();
            	for(int i=0; i<buttonStringList.length; i++) {
            		buttonGroup[i] = new JButton(buttonStringList[i]);
            		buttonGroup[i].addActionListener(muscleFrame);
            		buttonGroup[i].addActionListener(this);
            		buttonGroup[i].setActionCommand(buttonStringList[i]);
            		if (buttonStringList[i].length() < 10) { 
            			buttonGroup[i].setMinimumSize(MipavUtil.defaultButtonSize);
            			buttonGroup[i].setPreferredSize(MipavUtil.defaultButtonSize);
            		} else {
            			buttonGroup[i].setMinimumSize(MipavUtil.widenButtonSize);
            			buttonGroup[i].setPreferredSize(MipavUtil.widenButtonSize);
            		}
            		buttonGroup[i].setFont(MipavUtil.font12B);
            	
            		if (i < 3) {
            			topPanel.add(buttonGroup[i]);
            		} else {
            			bottomPanel.add(buttonGroup[i]);
            		}
            	}
            	buttonPanel.setLayout(new BorderLayout());
            	buttonPanel.add(topPanel, BorderLayout.NORTH);
            	buttonPanel.add(bottomPanel, BorderLayout.CENTER);
            	
            } else {
            
            	for(int i=0; i<buttonStringList.length; i++) {
            		buttonGroup[i] = new JButton(buttonStringList[i]);
            		buttonGroup[i].addActionListener(muscleFrame);
            		buttonGroup[i].addActionListener(this);
            		buttonGroup[i].setActionCommand(buttonStringList[i]);
            		if (buttonStringList[i].length() < 10) { 
            			buttonGroup[i].setMinimumSize(MipavUtil.defaultButtonSize);
            			buttonGroup[i].setPreferredSize(MipavUtil.defaultButtonSize);
            		} else {
            			buttonGroup[i].setMinimumSize(MipavUtil.widenButtonSize);
            			buttonGroup[i].setPreferredSize(MipavUtil.widenButtonSize);
            		}
            		buttonGroup[i].setFont(MipavUtil.font12B);
            	
            		buttonPanel.add(buttonGroup[i]);
            	}
            }
            return buttonPanel;
        }
    	
    	/**
         * Add a listener to this class so that when when the dialog has completed processing it can use notifyListener
         * to notify all listeners that the dialog has completed.
         *
         * @param  obj  AlgorithmInterface "object' to be added to the list
         */
        public void addListener(ActionListener obj) {
        	objectList.addElement(obj);
        }
        
        public abstract void actionPerformed(ActionEvent e);
    }

    private class VoiDialogPrompt extends DialogPrompt implements ActionListener {

        private final String[] buttonStringList = {OK, HIDE_ALL, CANCEL, RESET, HIDE_ONE};
    	
        private String objectName;
        
        private boolean closedVoi;
        
        private int numVoi;
        
        boolean voiExists;
        
        private JLabel selectText;
        
        /**Buffer for hide/show functions related to HIDE_ALL button**/
        private VOIVector voiBuffer;

        private JMenu propMenu;
        
        public VoiDialogPrompt(PlugInMuscleImageDisplay theParentFrame) {
            super(theParentFrame, "VOI");
           
            setButtons(buttonStringList);
            
            initDialog();
        }
        
        private boolean voiExists(String objectName) {     	
            String fileDir;
            if(multipleSlices)
            	fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"_"+getViewableSlice()+"\\";
            else
            	fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"\\";
            
            if(new File(fileDir+objectName+".xml").exists()) {
                return true;
            } 
            return false;
        }
        
        public void actionPerformed(ActionEvent e) {

            String command = e.getActionCommand();

            if(command.equals(HIDE_ALL)) {
                //clear all VOIs drawn
            	voiBuffer = getActiveImage().getVOIs();
                getActiveImage().unregisterAllVOIs();
                ((JButton)e.getSource()).setText(SHOW_ALL);
                ((JButton)e.getSource()).setActionCommand(SHOW_ALL);
                updateImages(true);
            } else if(command.equals(SHOW_ALL)) {
            	//show all VOIs previously drawn, do not get rid of any VOIs on screen
            	for(int i=0; i<voiBuffer.size(); i++)
            		getActiveImage().registerVOI(voiBuffer.get(i));
            	((JButton)e.getSource()).setText(HIDE_ALL);
                ((JButton)e.getSource()).setActionCommand(HIDE_ALL);
            } else if (command.equals(OK)) {
            
                    VOI goodVoi = checkVoi(); //check voi has correct number of curves, etc
                    if ( goodVoi != null ) { 
                        voiChangeState = true;
                        //save modified/created VOI to file
                        getActiveImage().unregisterAllVOIs();
                        getActiveImage().registerVOI(goodVoi);
                        String dir = getImageA().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
                        if(multipleSlices)
                        	dir += "_"+getViewableSlice()+"\\";
                        else
                        	dir += "\\";
                        saveAllVOIsTo(dir);

                        MipavUtil.displayInfo(objectName+" VOI saved in folder\n " + dir);
                        completed = true;
                        
                        getActiveImage().unregisterAllVOIs();
                        updateImages(true);
                    } else {
                    	completed = false;	//error state, does not return to main dialog, waits for user
                    }
                } else if (command.equals(CANCEL)) {
                	voiChangeState = true;
                } else if (command.equals(HELP)) {
                    PlugInMuscleSegmentation.showHelp("MS00001");
                } else if (command.equals(RESET)) {
                	getActiveImage().unregisterAllVOIs();
                	initVoiImage(activeTab); //replacing current image and updating
                	updateImages(true);
                	
                } else if (command.equals(HIDE_ONE)) {
                	VOIVector vec = getActiveImage().getVOIs();
                	VOI goodVoi = null;
                	for(int i=0; i<vec.size(); i++) {
                		if(vec.get(i).getName().equals(objectName))
                			goodVoi = vec.get(i);
                	} //note: does unregister/register to for faster method execution
                	getActiveImage().unregisterAllVOIs();
                    if(goodVoi != null)
                    	getActiveImage().registerVOI(goodVoi);
                    
                	updateImages(true);
                } else if (command.equals("PropVOIUp")) {
                	//TODO: Perform propogate  + smooth on one VOI here
                    if (componentImage.getVOIHandler().propVOI(1, false) == true) {
                        incSlice();
                    }
                } else if (command.equals("PropVOIDown")) {
                	//TODO: Perform propogate  + smooth on one VOI here
                    if (componentImage.getVOIHandler().propVOI(-1, false) == true) {
                        decSlice();
                    }
                } else if (command.equals("PropVOIAll")) {
                	//TODO: Perform propogate  + smooth on all VOIs here
                    componentImage.getVOIHandler().propVOIAll();
                } 
            }
            
        
        
        public void takeDownDialog() {
        	removeAll();
        }
        
        public void setUpDialog(String name, boolean closedVoi, int numVoi) {	
        	this.objectName = name;
        	this.closedVoi = closedVoi;
        	this.numVoi = numVoi;
        	
        	voiExists = voiExists(objectName);
        	
        	for(int i=0; i<buttonGroup.length; i++) {
	        	if(buttonGroup[i].getText().contains(HIDE_ONE)) {
	        		buttonGroup[i].setText(HIDE_ONE+" "+name.toLowerCase());
	        		buttonGroup[i].setPreferredSize(new Dimension(185, 30));
	        	}
	        }
            
            updateSelectionLabel();
        }
        

        public String getObjectName() {
            return objectName;
        }
        
        /**
         * Updates the selection label for each VOI.  The calling method will have updated closedVoi, numVoi, voiExists, and objectName
         */
        private void updateSelectionLabel() { 
            //Whether the user needs to select closed curves
        	String closedStr = closedVoi ? "closed " : "";
            //How many curves needed
            String pluralVOI = numVoi > 1 ? "s" : "";
            //Optional prefix
            String prefix = numVoi > 1 ? " up to " : " ";
            //Does the VOI exist
            String existStr = voiExists ? "Modify the" : "Create";
            
            String voiStr = new String(existStr+prefix+numVoi+" "+closedStr+"VOI curve"+pluralVOI+" around the "+
                                        objectName.toLowerCase()+".");
            
            selectText.setText(voiStr); //automatically updates
        }
        
        /**
         * Initializes the dialog box. Call updateSelectionLabel to change name
         *
         */
        protected void initDialog() {
            setForeground(Color.black);
            addNotify();    
            
            setLayout(new BorderLayout());
            
            JPanel mainPanel = new JPanel(new GridLayout(3, 3));
            
            mainPanel.setForeground(Color.black);
            mainPanel.setBorder(MipavUtil.buildTitledBorder("VOI Selection"));
            
            //GridBagConstraints gbc = new GridBagConstraints();
            //gbc.anchor = GridBagConstraints.CENTER;
            //gbc.fill = GridBagConstraints.HORIZONTAL;
            //gbc.gridx = 0;
            //gbc.gridy = 0;
            //gbc.ipadx = 0;
                
            //String propStr = new String("Use the propogating buttons below to allow MIPAV to generate similar VOIs "+
			//"on sequential slices.  First select the VOI you would like to propogate.");
            //JLabel propLabel = new JLabel(propStr);
            
            selectText = new JLabel("");
            
            selectText.setFont(MipavUtil.font12);
            mainPanel.add(selectText, BorderLayout.NORTH);
            //mainPanel.add(propLabel, BorderLayout.CENTER);
            
            JPanel buttonPanel = new JPanel();
            
            if(multipleSlices) {
            	JMenuItem item1, item2, item3;
            	propMenu = ViewMenuBuilder.buildMenu("Propogate", 0, true);
            	propMenu.add(item1 = ViewMenuBuilder.buildMenuItem("", "PropVOIDown", 0, this, "voipropd.gif", true));
            	propMenu.add(item2 = ViewMenuBuilder.buildMenuItem("", "PropVOIAll", 0, this, "voipropall.gif", true));
            	propMenu.add(item3 = ViewMenuBuilder.buildMenuItem("", "PropVOIUp", 0, this, "voipropu.gif", true));
            	item1.setToolTipText("Propagate VOI down");
            	item2.setToolTipText("Propagate VOI to all slices");
            	item3.setToolTipText("Propagate VOI up");
            	item1.setRolloverIcon(MipavUtil.getIcon("voipropdroll.gif"));
            	item2.setRolloverIcon(MipavUtil.getIcon("voipropallroll.gif"));
            	item3.setRolloverIcon(MipavUtil.getIcon("voipropuroll.gif"));
            	item1.setSize(new Dimension(40, 30));
            	item2.setSize(new Dimension(40, 30));
            	item3.setSize(new Dimension(40, 30));
            	buttonPanel.add(item1);
            	buttonPanel.add(item2);
            	buttonPanel.add(item3);
            	
            }
            
            buttonPanel.add(buildButtons(), BorderLayout.CENTER);

            add(mainPanel, BorderLayout.NORTH);
            add(buttonPanel, BorderLayout.CENTER);
            //gbc.gridy++;
        }
        
        

        /**
         * Determines whether a sufficient VOI has been created.
         * 
         * @return VOI created by user.  Null if no such VOI was created.
         */
        
        private VOI checkVoi() {
            VOIVector srcVOI = muscleFrame.getActiveImage().getVOIs();
            int countQualifiedVOIs = 0; //equal to numVoi when the right  amount of VOIs have been created
            VOI goodVOI = null;
            //see if the VOI has been modified
            for(int i=0; i<srcVOI.size(); i++) {
                if(srcVOI.get(i).getName().equals(objectName)) {
                    goodVOI = srcVOI.get(i);
                    countQualifiedVOIs++;
                }
            }
            if(countQualifiedVOIs != 1) {
	            //else VOI no longer exists, look for a VOI that doesn't fit to call objectName
	            for(int i=0; i<srcVOI.size(); i++) {
	            	if(getLocationStatus(srcVOI.get(i).getName()) == PlugInSelectableVOI.INVALID_LOCATION) {
	            		goodVOI = srcVOI.get(i);
	            		countQualifiedVOIs++;
	            	}
	            }
            }
            
            if(countQualifiedVOIs != 1) {
                String error = countQualifiedVOIs > 1 ? "You have created too many VOIs." : 
                                                                "You haven't created any VOIs.";
                MipavUtil.displayError(error);
                return null;  
            }
            Vector[] curves = goodVOI.getCurves();
            VOI voi = goodVOI;
            if(curves[getViewableSlice()].size() <= numVoi) {
                for(int i=0; i<numVoi; i++) {
                    if(closedVoi && voi.getCurveType() == VOI.CONTOUR) {
                        goodVOI.setName(objectName);
                        return goodVOI;
                    } else if(!closedVoi && voi.getCurveType() != VOI.CONTOUR) {
                        goodVOI.setName(objectName);
                        return goodVOI;
                    } 
                }
                String error = closedVoi ? "Any curves made must be closed." : 
                                            "Any curves made must be open.";
                MipavUtil.displayError(error);
            } 
            
            String error = "You have created too many curves.";
            MipavUtil.displayError(error);
            
            return null;
        }
        
    }
    
   

    private class MuscleDialogPrompt extends DialogPrompt {
        
        //~ Static fields/initializers -------------------------------------------------------------------------------------
    
        public static final int REMOVED_INTENSITY = -2048;
        
        public static final String CHECK_BOX = "CHECK_BOX";
        
        private final String[] buttonStringList = {CALCULATE, HELP, EXIT};
        
        //~ Instance fields ------------------------------------------------------------------------------------------------
        
        /** Denotes the anatomical part represented in the image. Implemented seperatly in case this class
         *  is moved to its own class at a later time.  
         */
        private ImageType imageType;
        
        /** Whether this image has mirror image muscles (eg VIEWS of thighs, abdomen. */
        private Symmetry symmetry;
        
        /** Labels for instructions. */
        private JLabel[] instructionLabel;
        
        private ColorButtonPanel[] mirrorCheckArr;
        
        /** Check boxes for non-mirror object buttons. */
        private ColorButtonPanel[] noMirrorCheckArr;

        /** Buttons for muscles where a mirror muscle may exist. */
        private JButton[] mirrorButtonArr;
        
        /** Buttons for muscles where mirror muscles are not considered. */
        private JButton[] noMirrorButtonArr;
        
        /** Text for muscles where a mirror muscle may exist. */
        private String[] mirrorArr;
        
        /** Text for muscles where mirror muscles are not considered. */
        private String[] noMirrorArr;
        
        //private TreeMap zeroStatus;

        private int index;
        
        /**
         * Creates new set of prompts for particular muscle.
         *
         * @param  theParentFrame  Parent frame.
         */
        public MuscleDialogPrompt(PlugInMuscleImageDisplay theParentFrame, String title, String[] mirrorArr, 
                String[] noMirrorArr, 
                ImageType imageType, Symmetry symmetry, int index) {
            //super(theParentFrame, false);
            super(theParentFrame, title);
            
            setButtons(buttonStringList);
            
            this.mirrorArr = mirrorArr;
            this.noMirrorArr = noMirrorArr;
            
            this.imageType = imageType;
            this.symmetry = symmetry;
            
            this.index = index;
            initDialog();    
        }
        
        public int getIndex() {
        	return index;
        }
        
        public void actionPerformed(ActionEvent e) {
            System.err.println(e.getActionCommand());
            System.out.println(e.getActionCommand());
        }
        
        private JPanel initInstructionPanel() {
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1;
            gbc.weighty = 1;
            gbc.gridx = 0;
            gbc.gridy = 0;
            
            JPanel instructionPanel = new JPanel(new GridBagLayout());
            instructionPanel.setForeground(Color.black);
            instructionPanel.setBorder(MipavUtil.buildTitledBorder("Instructions"));
            instructionLabel = new JLabel[4];
            instructionLabel[0] = new JLabel("1) Press an object button.\n\r");
            instructionLabel[1] = new JLabel("2) A dialog box will prompt you to draw VOI(s) around that object.");
            instructionLabel[2] = new JLabel("3) Once drawn the check box next to the button will be checked.");
            instructionLabel[3] = new JLabel("4) Press that button again to review your VOI(s).");
            
            for(int i=0; i<instructionLabel.length; i++) {
                instructionLabel[i].setFont(MipavUtil.font12);
                instructionPanel.add(instructionLabel[i], gbc);
                gbc.gridy++;
            }
            
            return instructionPanel;
        }
        
        public void clearButtons() {
        	for(int i=0; i<mirrorCheckArr.length; i++) {
        		mirrorCheckArr[i].setColor(Color.black);
        	}
        	for(int i=0; i<noMirrorCheckArr.length; i++) {
        		noMirrorCheckArr[i].setColor(Color.black);
        	}
        }
        
        private JPanel initSymmetricalObjects() {
            
            ButtonGroup mirrorGroup = new ButtonGroup();
            JPanel mirrorPanel = new JPanel(new GridBagLayout());
            mirrorPanel.setForeground(Color.black);
            String title = titles[index];
            String vowel = (title.charAt(0) == 'a' || 
            		title.charAt(0) == 'e' || 
            		title.charAt(0) == 'i' || 
            		title.charAt(0) == 'o' || 
            		title.charAt(0) == 'u') ? "n " : " ";
            int end = (title.charAt(title.length() - 1) == 's') ? 
            		title.length()-1 : title.length();
            title = title.toLowerCase().substring(0, end);
            
            mirrorPanel.setBorder(MipavUtil.buildTitledBorder("Select a"+vowel+title+" component"));
            
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1;
            gbc.weighty = 0;
            gbc.gridx = 0;
            gbc.gridy = 0;
            
            for(int i=0; i<mirrorArr.length * 2; i++) {
                String symmetry1 = "", symmetry2 = "";
                if(symmetry.equals(Symmetry.LEFT_RIGHT)) {
                    symmetry1 = "Left ";
                    symmetry2 = "Right ";
                } else if(symmetry.equals(Symmetry.TOP_BOTTOM)) {
                    symmetry1 = "Top ";
                    symmetry2 = "Bottom ";
                }
                       
                
                mirrorButtonArr[i] = (i % 2) == 0 ? new JButton(symmetry1+mirrorArr[i/2]) : 
                                                            new JButton(symmetry2+mirrorArr[i/2]);
                mirrorButtonArr[i].setFont(MipavUtil.font12B);
                mirrorButtonArr[i].setActionCommand(CHECK_VOI);
                mirrorButtonArr[i].addActionListener(muscleFrame);
                mirrorGroup.add(mirrorButtonArr[i]);

                mirrorCheckArr[i] = new ColorButtonPanel(Color.black, mirrorButtonArr[i].getText());        
               
                if(i != 0 && i % 2 == 0) {
                    gbc.gridy++;
                    gbc.gridx = 0;
                }
                gbc.weightx = 0;
                gbc.insets = new Insets(0, 5, 0, 0);
                mirrorPanel.add(mirrorCheckArr[i], gbc);
                gbc.insets = new Insets(0, 0, 0, 0);
                gbc.gridx++;
                gbc.weightx = 1;
                mirrorPanel.add(mirrorButtonArr[i], gbc);
                gbc.gridx++;
            }          
            return mirrorPanel;      
        }
        
        private JPanel initNonSymmetricalObjects() {

            ButtonGroup noMirrorGroup = new ButtonGroup();
            JPanel noMirrorPanel = new JPanel(new GridBagLayout());
            noMirrorPanel.setForeground(Color.black);
            noMirrorPanel.setBorder(MipavUtil.buildTitledBorder("Select an object"));
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = .5;
            gbc.weighty = 0;
            
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.gridwidth = 1;
            gbc.gridheight = 1;
            
            JPanel tempPanel = null;
            GridBagConstraints gbc2 = new GridBagConstraints();
          
            gbc2.fill = GridBagConstraints.HORIZONTAL;
            
            for(int i=0; i<noMirrorArr.length; i++) {
            	tempPanel = new JPanel(new GridBagLayout());
            	
                
                noMirrorButtonArr[i] = new JButton(noMirrorArr[i]);
                noMirrorButtonArr[i].setFont(MipavUtil.font12B);
                noMirrorButtonArr[i].setActionCommand(CHECK_VOI);
                noMirrorButtonArr[i].addActionListener(muscleFrame);
                noMirrorGroup.add(noMirrorButtonArr[i]);
              
                noMirrorCheckArr[i] = new ColorButtonPanel(Color.black, noMirrorArr[i]);
                
                gbc2.gridx = 0;
                gbc2.weightx = 0;
                gbc.insets = new Insets(0, 5, 0, 0);
                tempPanel.add(noMirrorCheckArr[i], gbc2);
                gbc.insets = new Insets(0, 0, 0, 0);
                gbc2.weightx = 1;
                gbc2.gridx++;
                tempPanel.add(noMirrorButtonArr[i], gbc2);
                
                gbc.gridx = 0;
                noMirrorPanel.add(tempPanel, gbc);
              
                gbc.fill = GridBagConstraints.BOTH;
                gbc.gridx++;
                noMirrorPanel.add(Box.createGlue(), gbc);
                
                gbc.gridy++;
            }
            
            return noMirrorPanel;
        }
        
        public void setButton(VOI v, String buttonName, Color c) {
        	for(int i=0; i<mirrorButtonArr.length; i++) {
        		if(buttonName.equals(mirrorButtonArr[i].getText())) {
        			mirrorCheckArr[i].setColor(c);
        			v.addVOIListener(mirrorCheckArr[i].getColorButton());
        		}
        	}
        	for(int i=0; i<noMirrorButtonArr.length; i++) {
        		if(buttonName.equals(noMirrorButtonArr[i].getText())) {
        			noMirrorCheckArr[i].setColor(c);
        			v.addVOIListener(noMirrorCheckArr[i].getColorButton());
        		}
        	}
        }
        
        protected void initDialog() {
            setForeground(Color.black);
            
            this.setLayout(new GridBagLayout());
            GridBagConstraints gbc = new GridBagConstraints();
            
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.anchor = GridBagConstraints.NORTHWEST;
            
            gbc.weightx = 1;
            gbc.weighty = 0;
            gbc.gridx = 0;
            gbc.gridy = 0;
            
            add(initInstructionPanel(), gbc);
            
            gbc.fill = GridBagConstraints.BOTH;
            gbc.weighty = 1;
            
            mirrorCheckArr = new ColorButtonPanel[mirrorArr.length * 2];
            mirrorButtonArr = new JButton[mirrorArr.length * 2];
            if(mirrorArr.length > 0) {
            	gbc.gridy++;
            	add(initSymmetricalObjects(), gbc);
            }
            
            noMirrorCheckArr = new ColorButtonPanel[noMirrorArr.length];
            noMirrorButtonArr = new JButton[noMirrorArr.length];
            if(noMirrorArr.length > 0) {
            	gbc.gridy++;
            	add(initNonSymmetricalObjects(), gbc);
            }
            	
            gbc.gridy++;
            gbc.fill = GridBagConstraints.BOTH;
            add(new JLabel(""), gbc);
            
            gbc.fill = GridBagConstraints.NONE;
            gbc.anchor = GridBagConstraints.SOUTH;
            gbc.gridy++;
            add(buildButtons(), gbc);                
        }
        
        public JButton[] getMirrorButton() {
        	if(mirrorButtonArr != null)
        		return mirrorButtonArr;
        	return new JButton[0];
        }
        
        public JButton[] getNoMirrorButton() {
        	if(noMirrorButtonArr != null)
        		return noMirrorButtonArr;
        	return new JButton[0];
        }

        public String[] getMirrorButtonArr() {
            String[] arr = new String[mirrorButtonArr.length];
            for(int i=0; i<arr.length; i++) {
                arr[i] = mirrorButtonArr[i].getText();
            }
            return arr;
        }
 
	    public String[] getNoMirrorButtonArr() {
	        String[] arr = new String[noMirrorButtonArr.length];
	        for(int i=0; i<arr.length; i++) {
	            arr[i] = noMirrorButtonArr[i].getText();
	        }
	        return arr;
	    }
    }
    
    private class AnalysisPrompt extends DialogPrompt implements ActionListener {
		
		public static final String LOAD_VOI = "Load VOI";
    	
    	private final String[] buttonStringList = {OUTPUT, OUTPUT_ALL, SAVE, TOGGLE_LUT, HELP, BACK};
	
		/**
		 * Labels for instructions. 
		 */
		private JLabel[] instructionLabel;

		/**
		 * Text for muscles where a mirror muscle may exist. 
		 */
		private String[][] mirrorArr;
		
		private String[] totalList;

		/**
		 * Text for muscles where mirror muscles are not considered. 
		 */
		private String[][] noMirrorArr;
		
		private ColorButtonPanel[][] mirrorCheckArr;
		
		private ColorButtonPanel[][] noMirrorCheckArr;
		
		private JButton[][] mirrorButtonArr;
		
		private JButton[][] noMirrorButtonArr;
		
		private int colorChoice = 0;
		
		private MuscleCalculation muscleCalc = new MuscleCalculation();

		private boolean lutOn = false;
		
		private TreeMap<String,ColorButtonPanel> checkBoxLocationTree;
		
		//Keeping as treeMap since expected size is so small, if number of muscles were greater than say 128, might use HashMap
		private Map <String,Double>totalAreaCalcTree, totalAreaCountTree, partialAreaTree, fatAreaTree, leanAreaTree;
		
		private Map <String,Double>meanFatHTree, meanLeanHTree, meanTotalHTree;
	
		/**
		 * Constructor, note is called at beginning of program, so mirrorArr and noMirrorArr
		 * may be used in threaded calculations.
		 * 
		 * @param theParentFrame
		 * @param mirrorArr
		 * @param noMirrorArr
		 */
		
		public AnalysisPrompt(PlugInMuscleImageDisplay theParentFrame, String[][] mirrorArr, String[][] noMirrorArr) {
	        super(theParentFrame, "Analysis");
	        
	        //even though done flag exists, synchronized just in case
	        totalAreaCalcTree = Collections.synchronizedMap(new TreeMap<String,Double>());
	        totalAreaCountTree = Collections.synchronizedMap(new TreeMap<String,Double>()); 
	        partialAreaTree = Collections.synchronizedMap(new TreeMap<String,Double>()); 
	        fatAreaTree = Collections.synchronizedMap(new TreeMap<String,Double>()); 
	        leanAreaTree = Collections.synchronizedMap(new TreeMap<String,Double>());
			
			meanFatHTree = Collections.synchronizedMap(new TreeMap<String,Double>());
			meanLeanHTree  = Collections.synchronizedMap(new TreeMap<String,Double>());
			meanTotalHTree = Collections.synchronizedMap(new TreeMap<String,Double>());
	        
	        setButtons(buttonStringList);

	        //by looking at calculations up here, this process can be done independent
	        //of the multiple symmetries that may exist within a given body.
	        this.noMirrorArr = getCalcItems(noMirrorArr);
	        this.mirrorArr = getCalcItems(mirrorArr);
	        
	        checkBoxLocationTree = new TreeMap();
	        
	        totalList = populateTotalList();
	        colorChoice = 0;
	        
	        initDialog();
	    }
		
		private String[][] getCalcItems(String[][] objectArr) {
			String[][] resultArr = new String[objectArr.length][];
			String tempStr = "";
			for(int i=0; i<objectArr.length; i++) {
				ArrayList<String> tempObj = new ArrayList();
				for(int j=0; j<objectArr[i].length; j++) 
					if(calcTree.get(tempStr = objectArr[i][j]).equals(true))
						tempObj.add(tempStr);
				resultArr[i] = new String[tempObj.size()];
				for(int j=0; j<resultArr[i].length; j++)
					resultArr[i][j] = tempObj.get(j);
			}
			return resultArr;
		}
		
		private void setButtons() {
			if (checkBoxLocationTree != null) {
				Set<String> keySet = checkBoxLocationTree.keySet();
				Iterator<String> it = keySet.iterator();
				while(it.hasNext()) {
					checkBoxLocationTree.get(it.next()).setColor(Color.black);
				}
			}
			for(int index=0; index<mirrorButtonArr.length; index++) {
				for(int i=0; i<mirrorButtonArr[index].length; i++) {
					mirrorButtonArr[index][i].setEnabled(voiExists(mirrorButtonArr[index][i].getText()));
				}
				for(int i=0; i<noMirrorButtonArr[index].length; i++) {
					noMirrorButtonArr[index][i].setEnabled(voiExists(noMirrorButtonArr[index][i].getText()));
				}
			}
		}
		
		private String[] populateTotalList() {
			int totalSize = 0;
    		for(int i=0; i<mirrorArr.length; i++) 
    			totalSize += mirrorArr[i].length*2;
    		for(int i=0; i<noMirrorArr.length; i++) 
    			totalSize += noMirrorArr[i].length;
    		String[] totalList = new String[totalSize];
    		int index = 0;
    		for(int i=0; i<mirrorArr.length; i++) {
    			for(int j=0; j<mirrorArr[i].length * 2; j++, index++) {
    				String symmetry1 = "", symmetry2 = "";
    				if(symmetry.equals(Symmetry.LEFT_RIGHT)) {
    					symmetry1 = "Left ";
    					symmetry2 = "Right ";
    				} else if(symmetry.equals(Symmetry.TOP_BOTTOM)) {
    					symmetry1 = "Top ";
    					symmetry2 = "Bottom ";
    				}
	            
    				totalList[index] = (j % 2) == 0 ? new String(symmetry1+mirrorArr[i][j/2]+".xml") : 
    													new String(symmetry2+mirrorArr[i][j/2]+".xml");
    			}
    		}
    		for(int i=0; i<noMirrorArr.length; i++) {
    			for(int j=0; j<noMirrorArr[i].length; j++, index++) {
    				totalList[index] = noMirrorArr[i][j]+".xml";
    			}
    		}
    		
	        return totalList;  
		}
		
		/**
		 * Loads the CT Thigh specific lut
		 *
		 */
		private void loadLUT() {
			float min = (float)getActiveImage().getMin();
			float max = (float)getActiveImage().getMax();
			
			TransferFunction transfer = new TransferFunction();
			transfer.addPoint(new Point2Df(min, 255));
			
			//fat = blue
			transfer.addPoint(new Point2Df(-190, 255));
			transfer.addPoint(new Point2Df(-190, 254));
			transfer.addPoint(new Point2Df(-30, 254));
			
			//partial = white
			transfer.addPoint(new Point2Df(-30, 5));
			transfer.addPoint(new Point2Df(0, 5));
			
			//muscle = red
			transfer.addPoint(new Point2Df(0, 0));
			transfer.addPoint(new Point2Df(100, 0));
			
			//rest is black
			transfer.addPoint(new Point2Df(100, 255));
			transfer.addPoint(new Point2Df(max, 255));
			
			getLUTa().makeCTThighTransferFunctions();
			getLUTa().setTransferFunction(transfer);
			getLUTa().makeLUT(256);
			
			VOIVector vec = getActiveImage().getVOIs();
			for(int i=0; i<vec.size(); i++) {
				if(getZeroStatus(vec.get(i).getName())) {
                	vec.get(i).setDisplayMode(VOI.SOLID);
                	vec.get(i).setOpacity((float)0.7);
                }
			}
			
			updateImages(true);
			getActiveImage().getParentFrame().updateImages(true);
			
			lutOn = true;
		}
		
		private void removeLUT() {
			
			ctMode(getActiveImage(), -175, 275);
			
			getLUTa().makeGrayTransferFunctions();
			getLUTa().makeLUT(256);
			
			VOIVector vec = getActiveImage().getVOIs();
			for(int i=0; i<vec.size(); i++) 
				vec.get(i).setDisplayMode(VOI.CONTOUR);
			
			updateImages(true);
			
			lutOn = false;
		}
		
		/**
	     * Initializes the dialog box.
	     *
	     */
	    
		protected void initDialog() {
	        setForeground(Color.black);
	        
	        JPanel instructionPanel = initInstructionPanel();
	        
	        JScrollPane mirrorPanel[] = new JScrollPane[mirrorArr.length];
	        
	        //JPanel noMirrorPanel[] = new JPanel[noMirrorArr.length];
	        
	        JPanel mainPanel = new JPanel();
	        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
	        
	        mainPanel.add(instructionPanel);
	        
	        //list = new JList[mirrorArr.length];
	        mirrorCheckArr = new ColorButtonPanel[mirrorArr.length][];
	        mirrorButtonArr = new JButton[mirrorArr.length][];
	        
	        noMirrorCheckArr = new ColorButtonPanel[noMirrorArr.length][];
	        noMirrorButtonArr = new JButton[noMirrorArr.length][];

	        String title = "";
	        //guaranteed (for now) that mirrorArr.length = noMirrorArr.length
	        for(int i=0; i<mirrorArr.length; i++) {
	        	
	        	JPanel subPanel = initSymmetricalObjects(i, title = titles[i]);
	        	initNonSymmetricalObjects(subPanel, i);
	        	
	        	mirrorPanel[i] = new JScrollPane(subPanel, 
	        										ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER,
		        									ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
	        	
	        	mirrorPanel[i].setForeground(Color.black);
		        String vowel = (title.charAt(0) == 'a' || 
	            		title.charAt(0) == 'e' || 
	            		title.charAt(0) == 'i' || 
	            		title.charAt(0) == 'o' || 
	            		title.charAt(0) == 'u') ? "n " : " ";
	            int end = (title.charAt(title.length() - 1) == 's') ? 
	            		title.length()-1 : title.length();
	            String subTitle = title.toLowerCase().substring(0, end);
	            
	            mirrorPanel[i].setBorder(MipavUtil.buildTitledBorder("View a"+vowel+subTitle+" component"));
	        		

	        	mainPanel.add(mirrorPanel[i]);
	        }
	        
	        mainPanel.add(buildButtons());
	        
	        for(int i=0; i<buttonGroup.length; i++) {
	        	if(buttonGroup[i].getText().equals(TOGGLE_LUT)) 
	        		buttonGroup[i].setText("Show LUT");
	        	else if(buttonGroup[i].getText().equals(OUTPUT)) {
	        		buttonGroup[i].setEnabled(false);
	        	} else if(buttonGroup[i].getText().equals(OUTPUT_ALL)) {
	        		buttonGroup[i].setEnabled(false);
	        	} else if(buttonGroup[i].getText().equals(SAVE)) {
	        		buttonGroup[i].setEnabled(false);
	        	}
	        }
	        
	        add(mainPanel, BorderLayout.CENTER);
	        
	    }
		
		private JPanel initInstructionPanel() {
	        GridBagConstraints gbc = new GridBagConstraints();
	        gbc.anchor = GridBagConstraints.NORTHWEST;
	        gbc.fill = GridBagConstraints.HORIZONTAL;
	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.weightx = 1;
	        gbc.weighty = 0;
	        
	        JPanel instructionPanel = new JPanel(new GridLayout(4, 1));
	        instructionPanel.setForeground(Color.black);
	        instructionPanel.setBorder(MipavUtil.buildTitledBorder("Instructions"));
	        instructionLabel = new JLabel[2];
	        instructionLabel[0] = new JLabel("1) Click on the muscle you would like to view.\n\r");
	        instructionLabel[1] = new JLabel("2) Check boxes indicate whether a particular VOI exists.");
	        //extra no longer needed for resizing
	        
	        for(int i=0; i<instructionLabel.length; i++) {
	            instructionLabel[i].setFont(MipavUtil.font12);
	            instructionPanel.add(instructionLabel[i], gbc);
	            gbc.gridy++;
	        }
	        
	        return instructionPanel;
	    }
		
		private JPanel initSymmetricalObjects(int index, String title) {

            mirrorCheckArr[index] = new ColorButtonPanel[mirrorArr[index].length * 2];
            mirrorButtonArr[index] = new JButton[mirrorArr[index].length * 2];
			JPanel subPanel = new JPanel(new GridBagLayout());
            subPanel.setForeground(Color.black);

			String[] mirrorString = new String[mirrorArr[index].length * 2];
	        
	        for(int i=0; i<mirrorArr[index].length * 2; i++) {
	            String symmetry1 = "", symmetry2 = "";
	            if(symmetry.equals(Symmetry.LEFT_RIGHT)) {
	                symmetry1 = "Left ";
	                symmetry2 = "Right ";
	            } else if(symmetry.equals(Symmetry.TOP_BOTTOM)) {
	                symmetry1 = "Top ";
	                symmetry2 = "Bottom ";
	            }
	            
	            mirrorString[i] = (i % 2) == 0 ? new String(symmetry1+mirrorArr[index][i/2]) : 
	                                                        new String(symmetry2+mirrorArr[index][i/2]);
	        }
	        
	        GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1;
            gbc.weighty = 0;
            gbc.gridx = 0;
            gbc.gridy = 0;
            
            for(int i=0; i<mirrorArr[index].length * 2; i++) {
                               
            	mirrorCheckArr[index][i] = new ColorButtonPanel(Color.BLACK, mirrorString[i]);
                
                checkBoxLocationTree.put(mirrorString[i], mirrorCheckArr[index][i]);
                
                mirrorButtonArr[index][i] = new JButton(mirrorString[i]);
                mirrorButtonArr[index][i].setEnabled(voiExists(mirrorString[i]));
                mirrorButtonArr[index][i].setFont(MipavUtil.font12B);
                mirrorButtonArr[index][i].setActionCommand(LOAD_VOI);
                mirrorButtonArr[index][i].addActionListener(this);
                
                if(i != 0 && i % 2 == 0) {
                    gbc.gridy++;
                    gbc.gridx = 0;
                }
                gbc.weightx = 0;
                gbc.insets = new Insets(0, 10, 0, 0);
                subPanel.add(mirrorCheckArr[index][i], gbc);
                gbc.insets = new Insets(0, 0, 0, 0);
                gbc.gridx++;
                gbc.weightx = 1;
                subPanel.add(mirrorButtonArr[index][i], gbc);
                gbc.gridx++;
                
            }          
	        
	        return subPanel;         
	    }
	    
	    private JPanel initNonSymmetricalObjects(JPanel subPanel, int index) {
      
	    	noMirrorCheckArr[index] = new ColorButtonPanel[noMirrorArr[index].length];
            noMirrorButtonArr[index] = new JButton[noMirrorArr[index].length];
	    	
	    	GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1;
            gbc.weighty = 0;
            gbc.gridx = 0;
            gbc.gridy = mirrorArr[index].length;
            
            for(int i=0; i<noMirrorArr[index].length; i++) {
                               
            	noMirrorCheckArr[index][i] = new ColorButtonPanel(Color.BLACK, noMirrorArr[index][i]);
                
                checkBoxLocationTree.put(noMirrorArr[index][i], noMirrorCheckArr[index][i]);
                
                noMirrorButtonArr[index][i] = new JButton(noMirrorArr[index][i]);
                noMirrorButtonArr[index][i].setEnabled(voiExists(noMirrorArr[index][i]));
                noMirrorButtonArr[index][i].setFont(MipavUtil.font12B);
                noMirrorButtonArr[index][i].setActionCommand(LOAD_VOI);
                noMirrorButtonArr[index][i].addActionListener(this);
                
                if(i != 0 && i % 2 == 0) {
                    gbc.gridy++;
                    gbc.gridx = 0;
                }
                gbc.weightx = 0;
                gbc.insets = new Insets(0, 10, 0, 0);
                subPanel.add(noMirrorCheckArr[index][i], gbc);
                gbc.insets = new Insets(0, 0, 0, 0);
                gbc.gridx++;
                gbc.weightx = 1;
                subPanel.add(noMirrorButtonArr[index][i], gbc);
                gbc.gridx++;   
            }          
	        
	        return subPanel;
	    }
	    
	    public void actionPerformed(ActionEvent e) {
	    	System.out.println("Caught 2: "+e.getActionCommand());
	    	String command = e.getActionCommand();
	        displayChanged = false;
	        if(command.equals(HIDE_ALL)) {
	            //clear all VOIs drawn
	        	muscleFrame.getImageA().unregisterAllVOIs();
	        	muscleFrame.updateImages();
	        } else {
	
	            if (command.equals(OK)) {
	                VOI goodVoi = null; //was checkVOI, why check exists
	                if (goodVoi != null) { 
	                    //save modified/created VOI to file
	                	muscleFrame.getImageA().unregisterAllVOIs();
	                	muscleFrame.getImageA().registerVOI(goodVoi);
	                	String dir;
	                	if(multipleSlices)
	                		dir = muscleFrame.getImageA().getImageDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"_"+getViewableSlice()+"\\";
	                	else
	                		dir = muscleFrame.getImageA().getImageDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"\\";
	                    muscleFrame.saveAllVOIsTo(dir);
	
	                    MipavUtil.displayInfo(/*objectName*/"test"+" VOI saved in folder\n " + dir);
	                    
	                    muscleFrame.getImageA().unregisterAllVOIs();
	                    muscleFrame.updateImages();
	                } else {
	                    //individual error messages are already displayed
	                }
	            } else if (command.equals(OUTPUT)) {
	            	System.err.println("imagepane size: " + imagePane.size());
	            	processCalculations(false, false);
	            } else if (command.equals(OUTPUT_ALL)) { 
	            	processCalculations(true, false);
	            } else if (command.equals(SAVE)) {
	            	setVisible(false);
	            	getActiveImage().getParentFrame().requestFocus();
	            	
	            	processCalculations(true, true);
	            	setVisible(true);
	            } else if (command.equals(TOGGLE_LUT)) {
	            	if(!lutOn) {
	            		loadLUT();
	            		((JButton)e.getSource()).setText("Hide LUT");
	            	} else {
	            		removeLUT();
	            		((JButton)e.getSource()).setText("Show LUT");
	            	}
	            } else if (command.equals(HELP)) {
	                PlugInMuscleSegmentation.showHelp("MS00040");
	            } else if (command.equals(LOAD_VOI)) {
	            	String text = ((JButton)e.getSource()).getText();
	            	VOIVector vec = getActiveImage().getVOIs();
	            	boolean exists = false;
	            	for(int i=0; i<vec.size(); i++) 
	            		if(vec.get(i).getName().equals(text)) {
	            			vec.get(i).removeVOIListener(checkBoxLocationTree.get(text).getColorButton());
	            			vec.remove(i);
	            			exists = true;
	            			checkBoxLocationTree.get(text).setColor(Color.BLACK);
	            			checkBoxLocationTree.get(text).repaint();
	            		}
	            	if(!exists) {
	            		VOI rec = getSingleVOI(text);
	            		Color c = null;
	            		if((c = voiMap.get(rec.getName()).getColor()).equals(PlugInSelectableVOI.INVALID_COLOR) && 
	            				(c = hasColor(rec)).equals(PlugInSelectableVOI.INVALID_COLOR)) {
	                		c = colorPick[colorChoice++ % colorPick.length];
	                	}
	            		rec.removeVOIListener(checkBoxLocationTree.get(text).getColorButton());
	            		rec.addVOIListener(checkBoxLocationTree.get(text).getColorButton());
	            		
	                	rec.setColor(c);
	            		rec.setThickness(2);
	            		
	            		if(lutOn && getZeroStatus(rec.getName())) {
	                    	rec.setDisplayMode(VOI.SOLID);
	                    	rec.setOpacity((float)0.7);
	                    }
	            		
	            		getActiveImage().registerVOI(rec);
	            	}
	            		
	            	updateImages(true);
	            }
	        }
	        
	    }
	    
	    /**
		 * Does calculations on each (or all) of the various areas, and either saves them
		 * to disk in a PDF or outputs in the Output-Data tab
		 * @param all whether to calculate all (true if PDF)
		 * @param doSave whether to save the output (and screen grabs) to a pdf
		 */
		private void processCalculations(boolean all, boolean doSave) {
			if(!muscleCalc.isFinished()) {
				//Note that since the buttons are disabled, this could only happen by being
				//directly called in the code
				MipavUtil.displayError("Still processing calculations.  Please try again");
				return;
			}
			
			boolean pdfCreated = false;
			
			//if PDF hasnt been created and we're saving, create it now
			if (doSave && !pdfCreated) {
				createPDF();
				pdfCreated = true;
			}
			Iterator itr;
			if(all)
				itr = fatAreaTree.keySet().iterator();
			else {
				ArrayList totalList = new ArrayList(), subList = new ArrayList();
				for (int listNum = 0; listNum < mirrorButtonArr.length; listNum++, subList = new ArrayList())  {   	
	    			for(int i=0; i<mirrorButtonArr[listNum].length; i++) 
	    				if(mirrorCheckArr[listNum][i].isSelected())
	    					subList.add(mirrorButtonArr[listNum][i].getText());
	    			totalList.addAll(subList);
		    	}
				for (int listNum = 0; listNum < noMirrorButtonArr.length; listNum++, subList = new ArrayList())  {   	
	    			for(int i=0; i<noMirrorButtonArr[listNum].length; i++) 
	    				if(noMirrorCheckArr[listNum][i].isSelected())
	    					subList.add(noMirrorButtonArr[listNum][i].getText());
	    			totalList.addAll(subList);
		    	}
	    		itr = totalList.iterator();
			}
				
			
			while(itr.hasNext()) {
				Object itrObj = itr.next();
				double totalAreaCalc = 0, totalAreaCount = 0, fatArea = 0, leanArea = 0;//, partialArea = 0;
				double meanFatH = 0, meanLeanH = 0, meanTotalH = 0;
				//pixels -> cm^2\
				if(totalAreaCalcTree.get(itrObj) != null) {
					
					totalAreaCalc = totalAreaCalcTree.get(itrObj);
					totalAreaCount = totalAreaCountTree.get(itrObj);
					fatArea = fatAreaTree.get(itrObj);
					leanArea = leanAreaTree.get(itrObj);
					meanFatH = meanFatHTree.get(itrObj);
					meanLeanH = meanLeanHTree.get(itrObj);
					meanTotalH = meanTotalHTree.get(itrObj);
					
					System.out.println("Compare areas: "+totalAreaCalc+"\tcount: "+totalAreaCount);
					
					if (doSave) {
						addToPDF((String)itrObj, fatArea, leanArea, totalAreaCount, meanFatH, meanLeanH, meanTotalH);
					} else {
						DecimalFormat dec = new DecimalFormat("0.#");
						String appMessage = itrObj+" calculations:\n"+"Fat Area: "+dec.format(fatArea)+
						"\t\t\t\tMean H: "+dec.format(meanFatH)+"\nLean Area: "+dec.format(leanArea)+
						"\t\t\tMean H: "+dec.format(meanLeanH)+"\nTotal Area: "+dec.format(totalAreaCount)+
						"\t\t\tMean H: "+dec.format(meanTotalH) + "\n\n";
					
						ViewUserInterface.getReference().getMessageFrame().append(appMessage, ViewJFrameMessage.DATA);
					}	
				}
			}
		
			if (doSave) {
					    		
				//now load all VOIs at once:
				ArrayList totalList = new ArrayList(), subList = new ArrayList();
				for (int listNum = 0; listNum < mirrorButtonArr.length; listNum++, subList = new ArrayList())  {   	
	    			for(int i=0; i<mirrorButtonArr[listNum].length; i++) 
	    				if(mirrorButtonArr[listNum][i].isEnabled())
	    					subList.add(mirrorButtonArr[listNum][i].getText());
	    			totalList.addAll(subList);
		    	}
				for (int listNum = 0; listNum < noMirrorButtonArr.length; listNum++, subList = new ArrayList())  {   	
	    			for(int i=0; i<noMirrorButtonArr[listNum].length; i++) 
	    				if(noMirrorButtonArr[listNum][i].isEnabled())
	    					subList.add(noMirrorButtonArr[listNum][i].getText());
	    			totalList.addAll(subList);
		    	}
				String[] allStrings = new String[totalList.size()];
				for(int i=0; i<totalList.size(); i++) 
	    			allStrings[i] = totalList.get(i) + ".xml";
				 
				loadVOIs(allStrings, .4);
			
				//loadLUT();
				updateImages(true);
				
				
				java.awt.Image edgeImage = captureImage();
				
				loadVOIs(new String[] {}, 0);
				loadLUT();
				java.awt.Image qaImage = captureImage();
				removeLUT();
				closePDF(edgeImage, qaImage);
				
				
				Set<String> keys = checkBoxLocationTree.keySet();
				Iterator<String> iter = keys.iterator();
				String voiName = null;
				while(iter.hasNext()) {
					voiName = iter.next();
        			checkBoxLocationTree.get(voiName).setColor(Color.BLACK);
        			checkBoxLocationTree.get(voiName).repaint();
				}
				
				
				updateImages(true);
				
				
				requestFocus();
			}	
		}
		
		private void enableCalcOutput() {
			for(int i=0; i<buttonGroup.length; i++) {
	        	if(buttonGroup[i].getText().equals(OUTPUT)) {
	        		buttonGroup[i].setEnabled(true);
	        	} else if(buttonGroup[i].getText().equals(OUTPUT_ALL)) {
	        		buttonGroup[i].setEnabled(true);
	        	} else if(buttonGroup[i].getText().equals(SAVE)) {
	        		buttonGroup[i].setEnabled(true);
	        	}
	        }
		}

		private void performCalculations() {
			getActiveImage().unregisterAllVOIs();
			updateImages(true);
	    	Thread calc = new Thread(muscleCalc);
	    	calc.start();
	    }
	    
		/**
		 * Performs required calculations for plugin. Partial voluming error ~2%
		 * 
		 * @author senseneyj
		 *
		 */
	    private class MuscleCalculation implements Runnable {
	    	private boolean done = false;
	    	
	    	public void run() {
	    		long time = System.currentTimeMillis();
	    		loadVOIs(totalList, 0);
	    		VOIVector vec = (VOIVector)getActiveImage().getVOIs().clone();
	    		getActiveImage().unregisterAllVOIs();
	    		for(int i=0; i<vec.size(); i++) {
	    			VOI v = vec.get(i);
	    			String name = v.getName();
	    			
	    			fatAreaTree.put(name, getFatArea(v)*(Math.pow(muscleFrame.getActiveImage().getResolutions(0)[0]*.1, 2)));
	    			partialAreaTree.put(name, getPartialArea(v)*(Math.pow(muscleFrame.getActiveImage().getResolutions(0)[0]*.1, 2)));
	    			leanAreaTree.put(name, getLeanArea(v)*(Math.pow(muscleFrame.getActiveImage().getResolutions(0)[0]*.1, 2)));
	    			totalAreaCalcTree.put(name, getTotalAreaCalc(v)*(Math.pow(muscleFrame.getActiveImage().getResolutions(0)[0]*.1, 2)));
	    			totalAreaCountTree.put(name, getTotalAreaCount(v)*(Math.pow(muscleFrame.getActiveImage().getResolutions(0)[0]*.1, 2)));
	    			
	    			meanFatHTree.put(name, getMeanFatH(v));
	    			meanLeanHTree.put(name, getMeanLeanH(v));
	    			meanTotalHTree.put(name, getMeanTotalH(v));
	    		}
	    		time = System.currentTimeMillis() - time;
	    		
	    		enableCalcOutput();
	    		
	    		getActiveImage().unregisterAllVOIs();
				updateImages(true);
	    		
	    		System.out.println("Finished in "+time);
	    		done = true;
	    	}
	    	
	    	public boolean isFinished() {
	    		return done;
	    	}

			public double getFatArea(VOI v) {
				int fatArea = 0;
				BitSet fullMask = new BitSet();
				v.createBinaryMask(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1]);
				double mark = 0;
				for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
			        mark = getImageA().getDouble(i);
					if(mark  >= -190 && mark <= -30) 
						fatArea++;
				}
				return fatArea;
			}

			public double getPartialArea(VOI v) {
				int partialArea = 0;
				BitSet fullMask = new BitSet();
				v.createBinaryMask(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1]);
				double mark = 0;
				for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
			        mark = getImageA().getDouble(i);
					if(mark  >= -30 && mark <= 0) 
						partialArea++;
				}
				return partialArea;
			}

			public double getLeanArea(VOI v) {
				int leanArea = 0;
				BitSet fullMask = new BitSet();
				v.createBinaryMask(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1]);
				double mark = 0;
				for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
			        mark = getImageA().getDouble(i);
					if(mark  >= 0 && mark <= 100) 
						leanArea++;
				}
				return leanArea;
			}

			//TODO: Partial voluming difference between VOI calculations and counting
			
			public double getTotalAreaCalc(VOI v) {
				return v.area();
			}

			public double getTotalAreaCount(VOI v) {
				int totalArea = 0;
				BitSet fullMask = new BitSet();
				v.createBinaryMask(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1]);
				for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) 
			        totalArea++;
				return totalArea;
			}

			public double getMeanFatH(VOI v) {
				int fatArea = 0;
				double meanFatH = 0;
				BitSet fullMask = new BitSet();
				v.createBinaryMask(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1]);
				double mark = 0;
				for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
			        mark = getImageA().getDouble(i);
					if(mark  >= -190 && mark <= -30) {
						fatArea++;
						meanFatH += mark;
					}
				}
				meanFatH /= fatArea;
				return meanFatH;
			}

			public double getMeanLeanH(VOI v) {
				int leanArea = 0;
				double meanLeanH = 0;
				BitSet fullMask = new BitSet();
				v.createBinaryMask(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1]);
				double mark = 0;
				for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
			        mark = getImageA().getDouble(i);
					if(mark  >= 0 && mark <= 100) {
						leanArea++;
						meanLeanH += mark;
					}
				}
				double testMean = meanLeanH;
				testMean /= leanArea;
				meanLeanH /= leanArea;
				return meanLeanH;
			}

			public double getMeanTotalH(VOI v) {
				int totalArea = 0;
				double meanTotalH = 0;
				BitSet fullMask = new BitSet();
				v.createBinaryMask(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1]);
				double mark = 0;
				for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
			        mark = getImageA().getDouble(i);
			        totalArea++;
					meanTotalH += mark;
				}
				meanTotalH /= totalArea;
				return meanTotalH;
			}
	    }
	}
    
    /**
     * Loads VOIs of all voiNames, 
     * @param voiName
     * @param fillVOIs
     */
    public void loadVOIs(String[] voiName, double fillVOIs) {
        getActiveImage().unregisterAllVOIs();
        int colorChoice = new Random().nextInt(colorPick.length);
        String fileDir;
        if(multipleSlices)
        	fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"_"+getViewableSlice()+"\\";
        else
        	fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"\\";
        File allVOIs = new File(fileDir);
        if(allVOIs.isDirectory()) {
            for(int i=0; i<voiName.length; i++) {
                //System.out.println(voiName[i]);

                if(new File(fileDir+voiName[i]).exists()) {
                    String fileName = voiName[i];
                    FileVOI v;
                    VOI[] voiVec = null;
                    try {
                        v = new FileVOI(fileName, fileDir, getActiveImage());
                        voiVec = v.readVOI(false);
                    } catch(IOException e) {
                        MipavUtil.displayError("Unable to load old VOI from location:\n"+fileDir+"\nWith name: "+fileName);
                    }
                    if(voiVec.length > 1) {
                        MipavUtil.displayError("Invalid VOI from location:\n"+fileDir+"\nWith name: "+fileName);
                    } else {
                    	if(voiMap.get(voiVec[0].getName()).getColor().equals(PlugInSelectableVOI.INVALID_COLOR)) {
	                    	Color c = hasColor(voiVec[0]);
	                        if(!(c = hasColor(voiVec[0])).equals(PlugInSelectableVOI.INVALID_COLOR))
	                            voiVec[0].setColor(c);
	                        else 
	                            voiVec[0].setColor(c = colorPick[colorChoice++ % colorPick.length]);
	                        voiMap.get(voiVec[0].getName()).setColor(c);
                    	} else
                    		voiVec[0].setColor(voiMap.get(voiVec[0].getName()).getColor());
                        voiVec[0].setThickness(2);
                        if(fillVOIs != 0 && getZeroStatus(voiVec[0].getName())) {
                        	voiVec[0].setDisplayMode(VOI.SOLID);
                        	voiVec[0].setOpacity((float)fillVOIs);
                        }
                        getActiveImage().registerVOI(voiVec[0]);
                    }
                }
            }
        }  
    }
    
    private Color hasColor(VOI voiVec) {
        Color c = PlugInSelectableVOI.INVALID_COLOR;
        VOIVector tempVec = getActiveImage().getVOIs();
        String side1 = "", side2 = ""; 
        if(Symmetry.LEFT_RIGHT == Symmetry.LEFT_RIGHT) {
        	side1 = "Left";
        	side2 = "Right";
        }
        for(int i=0; i<tempVec.size(); i++) {
            if(voiVec.getName().contains(side1) || voiVec.getName().contains(side2)) {
                if( !(tempVec.get(i).getName().contains(side1)  &&  voiVec.getName().contains(side1)) && 
                        !(tempVec.get(i).getName().contains(side2)  &&  voiVec.getName().contains(side2)) && 
                        tempVec.get(i).getName().endsWith(voiVec.getName().substring(voiVec.getName().indexOf(" ")))) {
                    c =  tempVec.get(i).getColor();
                }
            }
        }
        return c;
    }
    
    public boolean voiExists(String name) {
    	String fileDir;
    	if(multipleSlices)
    		fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"_"+getViewableSlice()+"\\";
    	else
    		fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"\\";
    	String ext = name.contains(".xml") ? "" : ".xml";
        
        if(new File(fileDir+name+ext).exists())
        	return true;
        return false;
    }
    
    /**
     * Deal with color on individual basis.  Loads VOI.
     * @param name
     * @return
     */
    public VOI getSingleVOI(String name) {
    	String fileDir;
    	if(multipleSlices)
    		fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"_"+getViewableSlice()+"\\";
    	else
    		fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"\\";
    	String ext = name.contains(".xml") ? "" : ".xml";
        
        if(new File(fileDir+name+ext).exists()) {
            FileVOI v;
            VOI[] voiVec = null;
            try {
                v = new FileVOI(name+ext, fileDir, getActiveImage());
                voiVec = v.readVOI(false);
            } catch(IOException e) {
                MipavUtil.displayError("Unable to load old VOI from location:\n"+fileDir+"\nWith name: "+name);
            }
            if(voiVec.length > 1) {
                MipavUtil.displayError("Invalid VOI from location:\n"+fileDir+"\nWith name: "+name);
            } else {
            	return voiVec[0];
            }
        }
        return null;
    }
        
    
	private class BuildThighAxes implements AlgorithmInterface {
		
	    private int zSlice;
	    
	    private ModelImage image;
	    
	    private boolean axesCompleted;
	    
	    private int[] defaultPts;
	    
	    private VOIVector VOIs;
	    
	    private int groupNum;
	    
	    private int thighIndex;
	    
	    private AlgorithmBSmooth[] smoothAlgo;
	    
	    private VOI[] thighVOIs;
	    
	    private boolean[] thighCompleted;
	    
	    public BuildThighAxes(ModelImage image, int _zSlice) {
	        this.zSlice = _zSlice;
	        this.image = image;
	        
	        smoothAlgo = new AlgorithmBSmooth[2];
	        
	        thighCompleted = new boolean[2];
	        thighCompleted[0] = false;
	        thighCompleted[1] = false;
	        
	        initThighAxes();
	    }
	
	    public void algorithmPerformed(AlgorithmBase algorithm) {
	        VOI resultVOI;
	        if(algorithm instanceof AlgorithmBSmooth) {
	            System.out.println("B Smooth completed");
	            if (smoothAlgo[thighIndex].isCompleted() == true && thighCompleted[thighIndex]) {
	
	                // The algorithm has completed and produced a
	                resultVOI = smoothAlgo[thighIndex].getResultVOI();
	                image.registerVOI(resultVOI);
	                //build axes here
	                axesCompleted = true;
	            }
	        }
	    }
	
	    public boolean getAxesCompleted() {
	        return axesCompleted;
	    }
	
	    private void initThighAxes() {
	        Vector[][] contours = new Vector[2][]; //either 2 or 3 dimensions
	        defaultPts = new int[2];
	        int nVOI;//, nContours;
	        float[] xPoints = null;
	        float[] yPoints = null;
	    
	        VOIs = image.getVOIs(); //note that VOIs must already be loaded
	    
	        nVOI = VOIs.size();
	    
	        if (nVOI == 0) {
	            return;
	        }
	        
	        thighVOIs = new VOI[2];
	        
	        for (groupNum = 0; groupNum < nVOI; groupNum++) {
	    
	            if (VOIs.get(groupNum).getName().equals("Left Thigh")) {
	                thighVOIs[0] = VOIs.get(groupNum);
	            }
	            else if (VOIs.get(groupNum).getName().equals("Right Thigh")) {
	                thighVOIs[1] = VOIs.get(groupNum);
	            }
	        }
	        
	        //No thighs found
	        if (groupNum == nVOI) {
	            MipavUtil.displayError("No whole thighs were found.  Cannot compute axes.  "+
	                                    "Please ensure that whole thighs are defined as seperate VOIs for this image.");
	            return;
	        }
	        
	        for(int i=0; i<thighVOIs.length; i++) {
	            
	            contours[i] = thighVOIs[i].getCurves();
	            //nContours = contours[i][zSlice].size();
	    
	            int elementNum = 0;
	       
	            Polygon[] gons = thighVOIs[i].exportPolygons(zSlice);
	
	            xPoints = new float[gons[elementNum].npoints + 5];
	            yPoints = new float[gons[elementNum].npoints + 5];
	
	            xPoints[0] = gons[elementNum].xpoints[gons[elementNum].npoints - 2];
	            yPoints[0] = gons[elementNum].ypoints[gons[elementNum].npoints - 2];
	
	            	xPoints[1] = gons[elementNum].xpoints[gons[elementNum].npoints - 1];
	            	yPoints[1] = gons[elementNum].ypoints[gons[elementNum].npoints - 1];
	            
	            for (i = 0; i < gons[elementNum].npoints; i++) {
	                xPoints[i + 2] = gons[elementNum].xpoints[i];
	                yPoints[52*i + 2] = gons[elementNum].ypoints[i];
	            }
	
	            xPoints[gons[elementNum].npoints + 2] = gons[elementNum].xpoints[0];
	            yPoints[gons[elementNum].npoints + 2] = gons[elementNum].ypoints[0];
	
	            xPoints[gons[elementNum].npoints + 3] = gons[elementNum].xpoints[1];
	            yPoints[gons[elementNum].npoints + 3] = gons[elementNum].ypoints[1];
	
	            xPoints[gons[elementNum].npoints + 4] = gons[elementNum].xpoints[2];
	            yPoints[gons[elementNum].npoints + 4] = gons[elementNum].ypoints[2];
	
	            AlgorithmArcLength arcLength = new AlgorithmArcLength(xPoints, yPoints);
	            defaultPts[i] = Math.round(arcLength.getTotalArcLength() / 6); //larger denom.
	        }
	    }
	    
	    public void createAxes() {
	        
	        for(int i=0; i<thighVOIs.length; i++) {
	    
	            try {
	    
	                // No need to make new image space because the user has chosen to replace the source image
	                // Make the algorithm class
	                smoothAlgo[i] = new AlgorithmBSmooth(image, thighVOIs[i], defaultPts[i], false);
	    
	                // This is very important. Adding this object as a listener allows the algorithm to
	                // notify this object when it has completed of failed. See algorithm performed event.
	                // This is made possible by implementing AlgorithmedPerformed interface
	                smoothAlgo[i].addListener(this);
	     
	                // Start the thread as a low priority because we wish to still have user interface.
	                if (smoothAlgo[i].startMethod(Thread.MIN_PRIORITY) == false) {
	                    MipavUtil.displayError("A thread is already running on this object");
	                }
	            } catch (OutOfMemoryError x) {
	                MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");
	        
	                return;
	            }
	        }
	    }
	}
		
	protected java.awt.Image captureImage() {
		getActiveImage().getParentFrame().requestFocus();
		Rectangle currentRectangle;
		Point p = new Point();
		p.x = 0;
	    p.y = 0;
	    
	    //grab the scrollpane (where image is displayed) location
	    SwingUtilities.convertPointToScreen(p, scrollPane.getViewport());
	
	    Dimension d = new Dimension();
	    d = scrollPane.getViewport().getSize();
	    
	    int width = (int)(getActiveImage().getFileInfo()[0].getExtents()[0] * getComponentImage().getZoomX());
	    int height = (int)(getActiveImage().getFileInfo()[0].getExtents()[1] * getComponentImage().getZoomY());
	    
	    if (d.height > height) {
	    	d.height = height;
	    } 
	    if (d.width > width) {
	    	d.width = width;
	    }
	    currentRectangle = new Rectangle(p, d); //p, d
	    
	    try {
	        Robot robot = new Robot();
	
	        return robot.createScreenCapture(currentRectangle);
	    } catch (OutOfMemoryError error) { //Results in no picture taken for PDF
	    } catch (AWTException error) { //Results in no picture taken for PDF
	    }
	    return null;
	}

	/**
	 * Adds the table of voi information to the pdf, adds the images (edge and QA), and closes the document
	 * @param edgeImage
	 * @param qaImage
	 */
	protected void closePDF(java.awt.Image edgeImage, java.awt.Image qaImage) {
			try {
			Paragraph aPar = new Paragraph();
			aPar.setAlignment(Element.ALIGN_CENTER);
			aPar.add(new Paragraph());
			aPar.add(aTable);
			pdfDocument.add(new Paragraph());
			pdfDocument.add(aPar);
			PdfPTable imageTable = new PdfPTable(2);
			imageTable.addCell("Edge Image");
			imageTable.addCell("QA Image");
			imageTable.addCell(Image.getInstance(edgeImage, null));
			
			imageTable.addCell(Image.getInstance(qaImage, null));
			
			Paragraph pImage = new Paragraph();
			pImage.add(new Paragraph());
			pImage.add(imageTable);
			pdfDocument.add(pImage);
			pdfDocument.close();
			
			MipavUtil.displayInfo("PDF saved to: " + pdfFile);
			ViewUserInterface.getReference().getMessageFrame().append("PDF saved to: " + pdfFile + "\n", ViewJFrameMessage.DATA);
			
			} catch (Exception e) {
				e.printStackTrace();
			}
			
		}

	/**
	 * Creates the PDF file and creates several tables for scanner information as well
	 * as the VOI statistic information
	 *
	 */
	protected void createPDF() {		
		String fileDir, fileName;
		if(multipleSlices) {
			fileDir = getActiveImage().getFileInfo(getViewableSlice()).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"_"+getViewableSlice()+"\\";
			fileName = fileDir + File.separator + "NIA_Report_" +getViewableSlice()+".pdf";
		} else {
			fileDir = getActiveImage().getFileInfo(getViewableSlice()).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"\\";
			fileName = fileDir + File.separator + "NIA_Report.pdf";
		}
		pdfFile = new File(fileName);
		if(pdfFile.exists()) {
			int i=0;
			while(pdfFile.exists() && i<1000) {
				if(multipleSlices)
					fileName = "NIA_Report_" +getViewableSlice()+"-"+(++i)+ ".pdf";
				else
					fileName = "NIA_Report-"+(++i)+ ".pdf";
				if(i == 1000) 
					MipavUtil.displayError("Too many PDFs have been created, overwriting "+fileName);
				pdfFile = new File(fileDir + File.separator + fileName);
			}
		}
		try {
			pdfDocument = new Document();
			pdfWriter = PdfWriter.getInstance(pdfDocument, new FileOutputStream(pdfFile));
			pdfDocument.addTitle(imageType+" Tissue Analysis Report");
			pdfDocument.addCreator("MIPAV: Muscle Segmentation");
			pdfDocument.open();
			
			
			Paragraph p = new Paragraph();
			
			//add the Title and subtitle
			p.setAlignment(Element.ALIGN_CENTER);
			p.add(new Chunk("MIPAV: Segmentation", new Font(Font.TIMES_ROMAN, 18)));
			p.add(new Paragraph());
			p.add(new Chunk(imageType+" Tissue Analysis Report", new Font(Font.TIMES_ROMAN, 12)));
			pdfDocument.add(new Paragraph(p));
			pdfDocument.add(new Paragraph(new Chunk("")));
			pdfDocument.add(new Paragraph());
			
			Font fontNormal = FontFactory.getFont("Helvetica", 10, Font.NORMAL, Color.DARK_GRAY);
			Font fontBold = FontFactory.getFont("Helvetica", 10, Font.BOLD, Color.BLACK);
	
			// Comment here to return paragraph /**
			MultiColumnText mct = new MultiColumnText(20);
			mct.setAlignment(Element.ALIGN_LEFT);
			mct.addRegularColumns(pdfDocument.left(), pdfDocument.right(), 10f, 4);
			mct.addElement(new Paragraph("Analyst:", fontBold));
			mct.addElement(new Paragraph("akoyama", fontNormal));
			mct.addElement(new Paragraph("Analysis Date:", fontBold));
			mct.addElement(new Paragraph("05/06/2007", fontNormal));
			pdfDocument.add(mct);
			
			MultiColumnText mct2 = new MultiColumnText(20);
			mct2.setAlignment(Element.ALIGN_LEFT);
			mct2.addRegularColumns(pdfDocument.left(), pdfDocument.right(), 10f, 4);
			mct2.addElement(new Paragraph("Name:", fontBold));
			mct2.addElement(new Paragraph(getActiveImage().getFileInfo()[getViewableSlice()].getFileName(), fontNormal));
			mct2.addElement(new Paragraph("Center:", fontBold));
			mct2.addElement(new Paragraph("Hjartavernd", fontNormal));
			pdfDocument.add(mct2);
			
			MultiColumnText mct3 = new MultiColumnText(20);
			mct3.setAlignment(Element.ALIGN_LEFT);
			mct3.addRegularColumns(pdfDocument.left(), pdfDocument.right(), 10f, 4);
			mct3.addElement(new Paragraph("ID:", fontBold));
			mct3.addElement(new Paragraph("Slice "+getViewableSlice(), fontNormal));
			mct3.addElement(new Paragraph("Scan Date:", fontBold));
			mct3.addElement(new Paragraph("05/09/2003", fontNormal));
			pdfDocument.add(mct3);
			
			//add the scanning parameters table
			PdfPTable spTable = new PdfPTable(2);
			PdfPCell cell = new PdfPCell(new Paragraph("Scanning Parameters"));
			cell.setHorizontalAlignment(Element.ALIGN_CENTER);
			cell.setColspan(2);
			spTable.addCell(cell);
			spTable.addCell("kVp:");
			spTable.addCell("120");
			spTable.addCell("mA:");
			spTable.addCell("213");
			spTable.addCell("Pixel Size:");
			spTable.addCell(Double.toString(getActiveImage().getResolutions(0)[0]*.1));
			spTable.addCell("Slice Thickness: (mm)");
			spTable.addCell(Float.toString(getActiveImage().getFileInfo()[getViewableSlice()].getSliceThickness()));
			spTable.addCell("Table Height: (cm)");
			spTable.addCell("143.00");
			
			Paragraph pTable = new Paragraph();
			pTable.add(new Paragraph());
			pTable.setAlignment(Element.ALIGN_CENTER);
			pTable.add(spTable);
			pdfDocument.add(new Paragraph(new Chunk("")));
			pdfDocument.add(pTable);
			
			// *// end commenting
			
			//create the Table where we will insert the data:
			aTable = new PdfPTable(new float[] {1.8f, 1f, 1f, 1f, 1f, 1f, 1f});
			
			// add Column Titles (in bold)
			aTable.addCell(new PdfPCell(new Paragraph("Area (cm^2)", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Total Area", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Fat Area", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Lean Area", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Fat HU", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Lean HU", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Total HU", fontBold)));
			
			
			return;
		} catch (Exception e) {
			return;
		}
	}
	
	/**
	 * Adds a row to the PDF VOI Table
	 * @param name the name of the area
	 * @param fatArea the amount of fat area
	 * @param leanArea amount of lean area
	 * @param totalAreaCount total area
	 * @param meanFatH mean fat area HU
	 * @param meanLeanH mean lean area HU
	 * @param meanTotalH mean total area HU
	 */
	protected void addToPDF(String name, double fatArea, double leanArea, double totalAreaCount, 
			double meanFatH, double meanLeanH, double meanTotalH) {
		
		try {
			Font fontNormal = FontFactory.getFont("Helvetica", 10, Font.NORMAL, Color.DARK_GRAY);
			if (name.endsWith(".xml")) {
				name = name.substring(0, name.length() - 4);
			}
			DecimalFormat dec = new DecimalFormat("0.##");
			
			//name of area
			aTable.addCell(new Paragraph( name, fontNormal) );
				
			//total area
			aTable.addCell(new Paragraph( dec.format(totalAreaCount), fontNormal) );
				
			//fat area
			aTable.addCell(new Paragraph( dec.format(fatArea), fontNormal) );
			
			//lean area
			aTable.addCell(new Paragraph( dec.format(leanArea), fontNormal) );
				
			//fat HU
			aTable.addCell(new Paragraph( dec.format(meanFatH), fontNormal) );
				
			//lean HU
			aTable.addCell(new Paragraph( dec.format(meanLeanH), fontNormal) );
				
			//total HU
			aTable.addCell(new Paragraph( dec.format(meanTotalH), fontNormal) );
			
			imageTable = new PdfPTable(2);
			imageTable.addCell("LUT Image");
			imageTable.addCell("VOI Image");
					
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * simple class that contains a ColorIcon for displaying the VOI color, and 
	 * has a button for changing the color of the linked VOI (linked by name)
	 * @author linkb
	 *
	 */
	private class ColorButtonPanel extends JPanel {
		
		private String voiName;
		private ColorButton colorButton;
		
		public ColorButtonPanel(Color c, String voiName) {
			super();
			this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
			//add(checkBox);
			this.voiName = voiName;
			colorButton = new ColorButton(c, voiName);
			colorButton.addActionListener(new ActionListener() { 
                public void actionPerformed(ActionEvent ae) {
                	if (colorButton.getColorIcon().getColor() != Color.BLACK) {
                		VOIVector vec = getActiveImage().getVOIs();
                		for(int i=0; i<vec.size(); i++) {
                			vec.get(i).setAllActive(false);
    	            		if(vec.get(i).getName().equals(getVOIName())) {
    	            			vec.get(i).setAllActive(true);
    	            		}
                		}
                		updateImages(true);
                		
                		getComponentImage().getVOIHandler().showColorDialog();
                	}
                }
            });
			add(colorButton);
		}
		
		public boolean isSelected() {
			if(colorButton.getColorIcon().getColor() == Color.BLACK) 
				return false;
			return true;
		}
		
		public String getVOIName() {
			return voiName;
		}
		
		public void setColor(Color c) {
			colorButton.getColorIcon().setColor(c);
		}
		
		public ColorButton getColorButton() {
			return colorButton;
		}
	}
	
	private class ColorButton extends JButton implements VOIListener {

		private ColorIcon cIcon;
		
		private String voiName;
		
		public ColorButton(Color c, String voiName) {
			super();
			cIcon = new ColorIcon(c, 13, 13);
			this.voiName = voiName;
			
			setIcon(cIcon);
			setForeground(ColorIcon.TRANSPARENT);
			setBackground(ColorIcon.TRANSPARENT);
			setBorder(null); 
			setSize(new Dimension(20,20));
			setPreferredSize(new Dimension(20,20));
		}
		
		public ColorIcon getColorIcon() {
			return cIcon;
		}
		
		public void addedCurve(VOIEvent added) {
            /* not interested in adding curves */
        }
        /**
         * We are not interested in removing Curves, so this method is empty.
         *
         * @param  added  DOCUMENT ME!
         */
        public void removedCurve(VOIEvent added) {
            /* not interested in removing curves */
        }

        /**
         * VOI Listener call (listens only to color changes)
         */
        public void colorChanged(Color c) {
        	cIcon.setColor(c);
        	voiMap.get(voiName).setColor(c);
        	this.repaint();
        }
        
        public void selectedVOI(VOIEvent selection) {
        	/* not interested in having the ColorButon select VOIs */
        }
	}
}
