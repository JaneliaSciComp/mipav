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
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.CustomUIBuilder.UIParams;
import gov.nih.mipav.view.dialogs.JDialogCaptureScreen;
import gov.nih.mipav.view.dialogs.JDialogWinLevel;

import com.lowagie.text.*;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.pdf.*;

import java.awt.*;
import java.awt.Rectangle;
import java.awt.event.*;
import java.awt.image.PixelGrabber;
import java.io.*;
import java.text.DecimalFormat;
import java.util.*;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class PlugInMuscleImageDisplay extends ViewJFrameImage {
    
    //~ Static fields --------------------------------------------------------------------------------------------------
    
    public static final Color[] colorPick = {Color.GREEN, Color.ORANGE, Color.CYAN, 
                                                Color.YELLOW, Color.MAGENTA};
    
    //~ Instance fields ------------------------------------------------------------------------------------------------    
    

    /**For writing PDF docs below. */
    private Document pdfDocument = null;
	private PdfWriter writer = null;
	private PdfPTable aTable = null;
	private PdfPTable imageTable = null;
    
    public static final String CHECK_VOI = "CHECK_VOI";
    
    public static final String VOI_DIR = "NIA_Seg\\";
    
    private int voiTabLoc;
    
    private int resultTabLoc;
    /**
     * Text for muscles where a mirror muscle may exist. 
     */
    private String[][] mirrorArr;

    private boolean[][] mirrorZ;

    /**
     * Text for muscles where mirror muscles are not considered. 
     */
    private String[][] noMirrorArr;

    private boolean[][] noMirrorZ;

    /**
     * Denotes the anatomical part represented in the image. Implemented seperatly in case this class is moved to its own class at a later time.
     */
    private ImageType imageType;

    /**
     * Whether this image has mirror image muscles (eg VIEWS of thighs, abdomen. 
     */
    private Symmetry symmetry;
    
    private JTabbedPane imagePane;
    
    private JSplitPane splitPane;
    
    private int activeTab;
    
    private DialogPrompt[] tabs;
    
    private TreeMap zeroStatus;
    
    private String[] titles; 
    
    private boolean displayChanged = false;
    
    private TreeMap locationStatus;
        
    public enum ImageType{
        
        /** denotes that the srcImg is an abdomen */
        ABDOMEN,
        
        /** denotes that the srcImg is two thighs */
        TWO_THIGHS,
        
        /** unknown image type, generally represents an error state */
        UNKNOWN
    }
    
    public enum Symmetry{
        
        /** Indicates the image has no symmetry. */
        NO_SYMMETRY, 
        
        /** Indicates that image has left-right symmetry. */
        LEFT_RIGHT, 
        
        /** Indicates the image has top-bottom symmetry. */
        TOP_BOTTOM
    }
   
    public PlugInMuscleImageDisplay(ModelImage image, String[] titles,
            String[][] mirrorArr, boolean[][] mirrorZ, 
            String[][] noMirrorArr, boolean[][] noMirrorZ,  
            ImageType imageType, Symmetry symmetry) {
    	super(image);
    	
    	this.setVisible(true);
        
        Preferences.setProperty(Preferences.PREF_CLOSE_FRAME_CHECK, "yes");
        
        this.setImageA(image);
        this.setActiveImage(IMAGE_A);
        this.titles = titles;
        this.mirrorArr = mirrorArr;
        this.mirrorZ = mirrorZ;
        this.noMirrorArr = noMirrorArr;
        this.noMirrorZ = noMirrorZ;
        this.imageType = imageType;
        this.symmetry = symmetry;
        
        locationStatus = new TreeMap();
        
        if (imageA == null) {
            return;
        }
        
        initNext();
    }
    
    public PlugInMuscleImageDisplay(ModelImage image, String[] titles,
            String[][] mirrorArr, boolean[][] mirrorZ, 
            String[][] noMirrorArr, boolean[][] noMirrorZ,  
            ImageType imageType, Symmetry symmetry, boolean standAlone) {

    	super(image, null, null, false, false);
    	this.setImageA(image);
        
        this.titles = titles;
        this.mirrorArr = mirrorArr;
        this.mirrorZ = mirrorZ;
        this.noMirrorArr = noMirrorArr;
        this.noMirrorZ = noMirrorZ;
        this.imageType = imageType;
        this.symmetry = symmetry;
        
        locationStatus = new TreeMap();
        
        Preferences.setProperty(Preferences.PREF_CLOSE_FRAME_CHECK, "yes");
        
    	initStandAlone(LUTa);
    	this.setActiveImage(IMAGE_A);
    	setVisible(true);
    }
    
    /**
     * Initializes the frame and variables.
     *
     * @param   LUTa           LUT of the imageA (if null grayscale LUT is constructed)
     * 
     * @throws  OutOfMemoryError  if enough memory cannot be allocated for the GUI
     */
    private void initStandAlone(ModelLUT LUTa) throws OutOfMemoryError {
        try {
            setIconImage(MipavUtil.getIconImage("davinci_32x32.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
        }

        setResizable(true);

        // initialize logMagDisplay
        this.LUTa = LUTa;

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
        

//      add pre-defined UIParams to the vector
        Vector<CustomUIBuilder.UIParams> voiParams = new Vector<CustomUIBuilder.UIParams>();
        voiParams.addElement(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER);
        voiParams.addElement(CustomUIBuilder.PARAM_VOI_LEVELSET);
        voiParams.addElement(CustomUIBuilder.PARAM_VOI_LIVEWIRE);
        voiParams.addElement(CustomUIBuilder.PARAM_VOI_ELLIPSE);
        voiParams.addElement(CustomUIBuilder.PARAM_VOI_RECTANGLE);
        
        Vector<CustomUIBuilder.UIParams> voiActionParams = new Vector<CustomUIBuilder.UIParams>();
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_NEW);
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_UNDO);
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_CUT);
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_COPY);
        voiActionParams.addElement(CustomUIBuilder.PARAM_VOI_PASTE);
       
        
        controls.buildSimpleToolBar();
        controls.addCustomToolBar(voiParams);
        controls.addCustomToolBar(voiActionParams);
        
        setTitle();
        
        // MUST register frame to image models
        imageA.addImageDisplayListener(this);

        windowLevel = new JDialogWinLevel[2];
	
        MipavUtil.centerOnScreen(this);
      
        updateImages(true);
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

        pack();
        
        // User interface will have list of frames
        userInterface.registerFrame(this);

//      System.err.println("adding controls....");
        getContentPane().add(controls, BorderLayout.NORTH);
        // getContentPane().add(ViewUserInterface.getReference().getMessageFrame().getTabbedPane(), BorderLayout.SOUTH);
  
        //setJMenuBar(menuBar);
        
        //call the normal init function here
        initNext();
    } // end init()
    
    /**
     * Cleans memory.
     *
     * @throws  Throwable  the <code>Exception</code> raised by this method
     */
    public void finalize() throws Throwable {
    	
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
    	
    	splitPane.removeAll();
    	splitPane = null;
    	
    	zeroStatus = null;
    	locationStatus = null;
    	System.gc();
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
        
        if(command.equals(PlugInMuscleImageDisplay.CHECK_VOI)) {
            ((VoiDialogPrompt)tabs[voiTabLoc]).setUpDialog(((JButton)(e.getSource())).getText(), true, 1);
            lockToPanel(voiTabLoc, "VOI"); //includes making visible
        } else if(command.equals(DialogPrompt.CALCULATE)) {
        	lockToPanel(resultTabLoc, "Analysis"); //includes making visible
        	((AnalysisPrompt)tabs[resultTabLoc]).performCalculations();
        } else if (!(command.equals(DialogPrompt.OUTPUT) ||
        		command.equals(DialogPrompt.SAVE) ||
        		command.equals(DialogPrompt.OUTPUT_ALL))) {
        	if(command.equals(DialogPrompt.CANCEL)) {
        		unlockToPanel(voiTabLoc);
        		initMuscleImage(activeTab);
        	} else if(command.equals(DialogPrompt.CLEAR)) {
        		getActiveImage().unregisterAllVOIs();
        		updateImages(true);
        	} else if(command.equals(DialogPrompt.OK) && 
        			tabs[voiTabLoc].completed() == true) {
        		unlockToPanel(voiTabLoc);
        		initMuscleImage(activeTab);
        	} else if(command.equals(DialogPrompt.BACK)) {
        		unlockToPanel(resultTabLoc);
        		initMuscleImage(activeTab);
        	} else if(command.equals(DialogPrompt.EXIT)) {
            	close();
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
        
        //next step would normally be to finalize, but we don't want the controls removed since the original image
        //may still be worked on

        System.gc();
    }
    
    private JPanel initDialog() {
    	//The component image will be displayed in a scrollpane.
        zeroStatus = new TreeMap();
        
        scrollPane = new JScrollPane(componentImage, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
        imagePane = new JTabbedPane();
        
        tabs = new DialogPrompt[mirrorArr.length+2]; //+2 for VOI and AnalysisPrompt
        
        JPanel panelA = new JPanel(new GridLayout(1, 3, 10, 10));
        
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, imagePane, scrollPane);
        scrollPane.setBackground(Color.black);
        panelA.add(splitPane);
     
        for(int i=0; i<mirrorArr.length; i++) { 
        	tabs[i] = new MuscleDialogPrompt(this, titles[i], mirrorArr[i], mirrorZ[i],
                    noMirrorArr[i], noMirrorZ[i],
                    imageType, symmetry);

        	tabs[i].addComponentListener(this);
        	tabs[i].addListener(this);
        	tabs[i].setName(titles[i]);
        	imagePane.addTab((i+1)+": "+titles[i], tabs[i]);
        	
        	zeroStatus.putAll(((MuscleDialogPrompt)tabs[i]).getZeroStatus());
        	
        	JButton[] mirror = ((MuscleDialogPrompt)tabs[i]).getMirrorButton();
            JButton[] noMirror = ((MuscleDialogPrompt)tabs[i]).getNoMirrorButton();
            for(int j=0; j<mirror.length; j++) {
                locationStatus.put(mirror[j].getText(), i);
            }
            for(int j=0; j<noMirror.length; j++) {
                locationStatus.put(noMirror[j].getText(), i);
            }
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
        
    	JPanel mainPanel = initDialog();
    	if (ViewUserInterface.getReference().isAppFrameVisible()) {
    		getContentPane().add(mainPanel, BorderLayout.CENTER);
    	} else {
    		JTabbedPane messageTabs = ViewUserInterface.getReference().getMessageFrame().getTabbedPane();
    		messageTabs.setPreferredSize(new Dimension(this.getWidth(), 100));
    		JSplitPane mainSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT, mainPanel, messageTabs );
    		mainSplit.setDividerLocation(550);
    		getContentPane().add(mainSplit, BorderLayout.CENTER);
    	}
        
        	
      
        
        if (ViewUserInterface.getReference().isAppFrameVisible()) {
        	getContentPane().remove(0);
        }

        pack();
        initMuscleImage(0);
        this.setResizable(true);
    }
    
    @Override
	public void componentShown(ComponentEvent event) {
	    Component c = event.getComponent();
	    if(c instanceof MuscleDialogPrompt) {
	    	for(int i=0; i<voiTabLoc; i++) {
	    		if(tabs[i].equals(c)) {
	    			initMuscleImage(i);
	    			activeTab = i;
	    			displayChanged = true;
	    		}
	    	}
	    } else if(c instanceof VoiDialogPrompt && activeTab != voiTabLoc && displayChanged != true) {
	    	initVoiImage(activeTab); //replacing current image and updating
	    	displayChanged = true;
	    } else if(c instanceof AnalysisPrompt && activeTab != resultTabLoc && displayChanged != true) {
	    	getActiveImage().unregisterAllVOIs();
	    	updateImages(true);
	    	displayChanged = true;
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
        //	System.err.println("Image size small");
        }
    
       // System.err.println("Current zoom: " + componentImage.getZoomX());

        
        
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
        updateImages(true);
    }
    
    /**
     * Identifies which panel a particular VOI belongs to
     * 
     * @param name the VOI
     * @return a panel where 0 is first
     */
    public int getLocationStatus(String name) {
    	int loc = -1;
    	if(locationStatus.get(name) != null)
    		loc = (Integer)locationStatus.get(name);
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
    	if(zeroStatus.get(name) != null)
    		fill = (Boolean)zeroStatus.get(name);
    	return fill;
    }

    private void loadVOI(int pane) {
        int colorChoice = 0;
    	getImageA().unregisterAllVOIs();
        String fileDir = getImageA().getFileInfo(0).getFileDirectory()+VOI_DIR;
        File allVOIs = new File(fileDir);
        //ArrayList paneVOIs = new ArrayList();
        if(allVOIs.isDirectory()) {
            String[] voiName = allVOIs.list();
            for(int i=0; i<voiName.length; i++) {
            	//voiName[i] = voiName[i].substring(0, voiName[i].indexOf(".xml"));
            	//if(getLocationStatus(voiName[i]) == pane)
            	//	paneVOIs.add(voiName[i]);

            	String name = voiName[i].substring(0, voiName[i].indexOf(".xml"));
            	String ext = ".xml";
            	VOI v;
            	if((Integer)locationStatus.get(name) == pane) {
            		v = getSingleVOI(name+ext);
            		if(v != null) {
            			v.setThickness(2);
            			Color c = null;
                    	if((c = hasColor(v)) == null)
                            v.setColor(colorPick[colorChoice++ % colorPick.length]);
                    	else
                    		v.setColor(c);
                    	v.setDisplayMode(VOI.CONTOUR);
            			getActiveImage().registerVOI(v);
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
        loadVOI(pane);
        
        ctMode(getImageA(), -175, 275);
    
        VOIVector vec = getImageA().getVOIs();
    	for(int i=0; i<vec.size(); i++) {            
    		for(int j=0; j<tabs.length; j++) {
    			if(tabs[j] instanceof MuscleDialogPrompt) {
    				if(((MuscleDialogPrompt)tabs[j]).hasButton(vec.get(i).getName())) {
    					((MuscleDialogPrompt)tabs[j]).setButton(vec.get(i).getName());
    				}	
    			}
    		}
    	}

        updateImages(true);
    }
    
    private void initVoiImage(int pane) {
    	getActiveImage().unregisterAllVOIs();
    	//load VOIs of activeTab
    	loadVOI(pane);
        VOIVector voiVec = getActiveImage().getVOIs();
    	for(int i=0; i<voiVec.size(); i++) {
    		VOI voi = voiVec.get(i);
    		if((Boolean)zeroStatus.get(voi.getName()) && 
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
    	
    	public static final String DIALOG_COMPLETED = "Dialog Completed";
    	
    	public static final String OK = "Ok";
    	public static final String CANCEL = "Cancel";
    	public static final String CLEAR = "Clear";
    	public static final String EXIT = "Exit";
    	public static final String HELP = "Help";
    	public static final String CALCULATE = "Calculate";
    	public static final String SAVE = "Save";
    	public static final String OUTPUT_ALL = "Output All";
    	public static final String TOGGLE_LUT = "Toggle LUT";
    	public static final String OUTPUT = "Output";
    	public static final String BACK = "Back";
    	
    	private String buttonStringList[] = {OK, CLEAR, HELP};
    	
    	protected JButton buttonGroup[];
    	
    	protected PlugInMuscleImageDisplay muscleFrame;
    	
    	private Vector objectList = new Vector();
    	
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
            	buttonPanel.add(bottomPanel, BorderLayout.SOUTH);
            	
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
        
        /**
         * Used to notify all listeners that the pseudo-algorithm has completed.
         *
         * @param  dialog the sub-dialog that has completed the function
         */
        public void notifyListeners(String cmd) {

            for (int i = 0; i < objectList.size(); i++) {
                ((ActionListener) objectList.elementAt(i)).actionPerformed(new ActionEvent(this, 0, cmd));
            }
        }
        
        public abstract void actionPerformed(ActionEvent e);
    }

    private class VoiDialogPrompt extends DialogPrompt implements ActionListener {

        private final String[] buttonStringList = {OK, CLEAR, CANCEL};
    	
        private String objectName;
        
        private boolean closedVoi;
        
        private int numVoi;
        
        boolean voiExists;
        
        private JLabel selectText;

        public VoiDialogPrompt(PlugInMuscleImageDisplay theParentFrame) {
            super(theParentFrame, "VOI");
           
            setButtons(buttonStringList);
            
            initDialog();
        }
        
        private boolean voiExists(String objectName) {
        	
        	String fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
            
            if(new File(fileDir+objectName+".xml").exists()) {
                return true;
            } 
            return false;
        }
        
        public void actionPerformed(ActionEvent e) {

            String command = e.getActionCommand();

            if(command.equals(CLEAR)) {
                //clear all VOIs drawn
                getActiveImage().unregisterAllVOIs();
                updateImages(true);
            } else if (command.equals(OK)) {
                    VOI goodVoi = checkVoi();
                    //check that VOI conforms to requirements, returns the VOI being modified/created
                    if ( goodVoi != null ) { 
                        
                        //save modified/created VOI to file
                        getActiveImage().unregisterAllVOIs();
                        getActiveImage().registerVOI(goodVoi);
                        String dir = getActiveImage().getImageDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
                        saveAllVOIsTo(dir);
                        
                        String fileDir = getActiveImage().getFileInfo(0).getFileDirectory();

                        MipavUtil.displayInfo(objectName+" VOI saved in folder\n " + fileDir + PlugInMuscleImageDisplay.VOI_DIR);
                        completed = true;
                        
                        getActiveImage().unregisterAllVOIs();
                        updateImages(true);
                        
                        notifyListeners(OK);
                    } else {
                    	//Note that no VOI has been saved due to previous error.  will not return to main dialog.
                    	completed = false;
                    }
                } else if (command.equals(CANCEL)) {
                    notifyListeners(CANCEL);
                    //dispose();
                } else if (command.equals(HELP)) {
                    MipavUtil.showHelp("19014");
                    
                    
                    
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
            //Does the VOI exist
            String existStr = voiExists ? "Modify the" : "Create";
            
            String voiStr = new String(existStr+" "+numVoi+" "+closedStr+"VOI curve"+pluralVOI+" around the "+
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
            
            JPanel mainPanel = new JPanel(new GridLayout(1, 1));
            
            mainPanel.setForeground(Color.black);
            mainPanel.setBorder(MipavUtil.buildTitledBorder("VOI Selection"));
            
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.ipadx = 0;
                
            selectText = new JLabel("");
            
            selectText.setFont(MipavUtil.font12);
            mainPanel.add(selectText, BorderLayout.NORTH);
            
            add(mainPanel, BorderLayout.NORTH);
            gbc.gridy++;
            add(buildButtons(), BorderLayout.CENTER);
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
	            	if(getLocationStatus(srcVOI.get(i).getName()) == -1) {
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
            if(curves[0].size() == numVoi) {
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
            } else {
                String error = curves[0].size() > numVoi ? "You have created too many curves." : 
                                                                    "You haven't created enough curves.";
                MipavUtil.displayError(error);
            }
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
        
        /** Check boxes for mirror muscle buttons. */
        private JCheckBox[] mirrorCheckArr;
        
        /** Check boxes for non-mirror object buttons. */
        private JCheckBox[] noMirrorCheckArr;
        
        /** Buttons for muscles where a mirror muscle may exist. */
        private JButton[] mirrorButtonArr;
        
        /** Buttons for muscles where mirror muscles are not considered. */
        private JButton[] noMirrorButtonArr;
        
        /** Text for muscles where a mirror muscle may exist. */
        private String[] mirrorArr;
        
        private boolean[] mirrorZ;
        
        /** Text for muscles where mirror muscles are not considered. */
        private String[] noMirrorArr;
        
        private boolean[] noMirrorZ;
        
        private TreeMap zeroStatus;

        
        /**
         * Creates new set of prompts for particular muscle.
         *
         * @param  theParentFrame  Parent frame.
         */
        public MuscleDialogPrompt(PlugInMuscleImageDisplay theParentFrame, String title, String[] mirrorArr, boolean[] mirrorZ, 
                String[] noMirrorArr, boolean[] noMirrorZ,  
                ImageType imageType, Symmetry symmetry) {
            //super(theParentFrame, false);
            super(theParentFrame, title);
            
            setButtons(buttonStringList);
            
            this.mirrorArr = mirrorArr;
            this.noMirrorArr = noMirrorArr;
            
            this.mirrorZ = mirrorZ;
            this.noMirrorZ = noMirrorZ;
            
            this.imageType = imageType;
            this.symmetry = symmetry;
            
            initDialog();    
        }
        
        public boolean hasButton(String buttonText) {
            if(zeroStatus.get(buttonText) != null) {
                return true;
            } 
            return false;
        }
        
        public void setButton(String buttonText) {
            for(int i=0; i<mirrorButtonArr.length; i++) {
                if(mirrorButtonArr[i].getText().equals(buttonText)) {
                    mirrorCheckArr[i].setSelected(true);
                }
            }
            for(int i=0; i<noMirrorButtonArr.length; i++) {
                if(noMirrorButtonArr[i].getText().equals(buttonText)) {
                    noMirrorCheckArr[i].setSelected(true);
                }
            }
        }
        
        public void actionPerformed(ActionEvent e) {
            
        }
        
        public TreeMap getZeroStatus() {
            return zeroStatus;
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
        
        private JPanel initSymmetricalObjects() {
            
            VOIVector existingVois = muscleFrame.getImageA().getVOIs();
            zeroStatus = new TreeMap();
             
            mirrorCheckArr = new JCheckBox[mirrorArr.length * 2];
            mirrorButtonArr = new JButton[mirrorArr.length * 2];
            ButtonGroup mirrorGroup = new ButtonGroup();
            JPanel mirrorPanel = new JPanel(new GridBagLayout());
            mirrorPanel.setForeground(Color.black);
            mirrorPanel.setBorder(MipavUtil.buildTitledBorder("Select an object"));
            
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
                mirrorCheckArr[i] = new JCheckBox();
                mirrorCheckArr[i].setEnabled(false);
                mirrorCheckArr[i].setHorizontalAlignment(SwingConstants.RIGHT);
                
                
                mirrorButtonArr[i] = (i % 2) == 0 ? new JButton(symmetry1+mirrorArr[i/2]) : 
                                                            new JButton(symmetry2+mirrorArr[i/2]);
                mirrorButtonArr[i].setFont(MipavUtil.font12B);
                mirrorButtonArr[i].setActionCommand(PlugInMuscleImageDisplay.CHECK_VOI);
                mirrorButtonArr[i].addActionListener(muscleFrame);
                mirrorGroup.add(mirrorButtonArr[i]);
                
                for(int j=0; j<existingVois.size(); j++) {
                    if(existingVois.get(j).getName().equals(mirrorButtonArr[i].getText())) {
                        mirrorCheckArr[i].setSelected(true);
                    }
                }
                
                if(i != 0 && i % 2 == 0) {
                    gbc.gridy++;
                    gbc.gridx = 0;
                }
                gbc.weightx = 0;
                mirrorPanel.add(mirrorCheckArr[i], gbc);
                gbc.gridx++;
                gbc.weightx = 1;
                mirrorPanel.add(mirrorButtonArr[i], gbc);
                gbc.gridx++;
                
                zeroStatus.put(mirrorButtonArr[i].getText(), mirrorZ[i/2]);
            }          
            return mirrorPanel;      
        }
        
        private JPanel initNonSymmetricalObjects() {
            VOIVector existingVois = muscleFrame.getImageA().getVOIs();
            
            noMirrorCheckArr = new JCheckBox[noMirrorArr.length];
            noMirrorButtonArr = new JButton[noMirrorArr.length];
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
                noMirrorCheckArr[i] = new JCheckBox();
                noMirrorCheckArr[i].setEnabled(false);
                noMirrorCheckArr[i].setHorizontalAlignment(SwingConstants.RIGHT);
                
                noMirrorButtonArr[i] = new JButton(noMirrorArr[i]);
                noMirrorButtonArr[i].setFont(MipavUtil.font12B);
                noMirrorButtonArr[i].setActionCommand(PlugInMuscleImageDisplay.CHECK_VOI);
                noMirrorButtonArr[i].addActionListener(muscleFrame);
                noMirrorGroup.add(noMirrorButtonArr[i]);
              
                gbc2.gridx = 0;
                gbc2.weightx = 0;
                tempPanel.add(noMirrorCheckArr[i], gbc2);
                
                gbc2.weightx = 1;
                gbc2.gridx++;
                tempPanel.add(noMirrorButtonArr[i], gbc2);
                
                gbc.gridx = 0;
                noMirrorPanel.add(tempPanel, gbc);
              
                gbc.fill = GridBagConstraints.BOTH;
                gbc.gridx++;
                noMirrorPanel.add(Box.createGlue(), gbc);
                
                //noMirrorPanel.add(noMirrorCheckArr[i], gbc);
                //gbc.gridx++;
                //gbc.weightx = 1;
                //noMirrorPanel.add(noMirrorButtonArr[i], gbc);
                
                gbc.gridy++;
                for(int j=0; j<existingVois.size(); j++) {
                    if(existingVois.get(j).getName().equals(noMirrorButtonArr[i].getText())) {
                        noMirrorCheckArr[i].setSelected(true);
                    }
                }
                
                //System.out.println(noMirrorButtonArr[i].getText()+" is "+noMirrorZ[i]);
                zeroStatus.put(noMirrorButtonArr[i].getText(), noMirrorZ[i]);
            }
            
            return noMirrorPanel;
        }
        
        protected void initDialog() {
            setForeground(Color.black);
            zeroStatus = new TreeMap();
            
            JPanel instructionPanel = initInstructionPanel();
            
            JPanel mirrorPanel = initSymmetricalObjects();
            
            JPanel noMirrorPanel = initNonSymmetricalObjects();
            this.setLayout(new GridBagLayout());
            GridBagConstraints gbc = new GridBagConstraints();
            
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.anchor = GridBagConstraints.NORTHWEST;
            
            gbc.weightx = 1;
            gbc.weighty = 0;
            gbc.gridx = 0;
            gbc.gridy = 0;
            
            add(instructionPanel, gbc);
            
            gbc.fill = GridBagConstraints.BOTH;
            gbc.weighty = 1;
            gbc.gridy++;
            add(mirrorPanel, gbc);
    
            gbc.gridy++;
            add(noMirrorPanel, gbc);
    
            gbc.gridy++;
            gbc.fill = GridBagConstraints.BOTH;
            add(new JLabel(""), gbc);
            
            gbc.fill = GridBagConstraints.NONE;
            gbc.anchor = GridBagConstraints.SOUTH;
            gbc.gridy++;
            add(buildButtons(), gbc);                
        }
        
        public TreeMap getIdentifiers() {
            return zeroStatus;
        }
        
        public JButton[] getMirrorButton() {
            return mirrorButtonArr;
        }
        
        public JButton[] getNoMirrorButton() {
            return noMirrorButtonArr;
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
    
    private class AnalysisPrompt extends DialogPrompt implements ActionListener, ListSelectionListener {
		
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
		
		private JList[] list;
		
		private String name[] = {"thigh", "bone component", "muscle"};
		
		private int time = 0;
		
		private MuscleCalculation muscleCalc = new MuscleCalculation();

		private boolean lutOn = false;
		
		//Keeping as treeMap since expected size is so small, if number of muscles were greater than say 128, might use HashMap
		private Map totalAreaCalcTree, totalAreaCountTree, partialAreaTree, fatAreaTree, leanAreaTree;
		
		private Map meanFatHTree, meanLeanHTree, meanTotalHTree;
	
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
	        totalAreaCalcTree = Collections.synchronizedMap(new TreeMap());
	        totalAreaCountTree = Collections.synchronizedMap(new TreeMap()); 
	        partialAreaTree = Collections.synchronizedMap(new TreeMap()); 
	        fatAreaTree = Collections.synchronizedMap(new TreeMap()); 
	        leanAreaTree = Collections.synchronizedMap(new TreeMap());
			
			meanFatHTree = Collections.synchronizedMap(new TreeMap());
			meanLeanHTree  = Collections.synchronizedMap(new TreeMap());;
			meanTotalHTree = Collections.synchronizedMap(new TreeMap());
	        
	        setButtons(buttonStringList);
	        
	        this.noMirrorArr = noMirrorArr;
	        this.mirrorArr = mirrorArr;
	        
	        totalList = populateTotalList();
	        
	        initDialog();
	    }
		
		private String[] populateTotalList() {
			int totalSize = 0;
    		for(int i=0; i<mirrorArr.length; i++) 
    			totalSize += mirrorArr[i].length*2;
    		String[] totalList = new String[totalSize];

    		for(int i=0, index=0; i<mirrorArr.length; i++) {
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
	        
	        list = new JList[mirrorArr.length];
	        
	        for(int i=0; i<mirrorArr.length; i++) {
	        	mirrorPanel[i] = initSymmetricalObjects(i, name[i]);
	        	mainPanel.add(mirrorPanel[i]);
	        }
	        
	        //for(int i=0; i<noMirrorArr.length; i++) {
	        //	initNonSymmetricalObjects(i);
	        //	mainPanel.add(noMirrorPanel[i]);
	        //}
	
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
	        instructionLabel[0] = new JLabel("1) Click on the muscle you would like to \n\r");
	        instructionLabel[1] = new JLabel("2) Highlight a VOI to view its measurements.");
	        //extra no longer needed for resizing
	        
	        for(int i=0; i<instructionLabel.length; i++) {
	            instructionLabel[i].setFont(MipavUtil.font12);
	            instructionPanel.add(instructionLabel[i], gbc);
	            gbc.gridy++;
	        }
	        
	        return instructionPanel;
	    }
		
		private JScrollPane initSymmetricalObjects(int index, String title) {
	         
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
	        
	        list[index] = new JList(mirrorString);
	        list[index].setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
	        list[index].setLayoutOrientation(JList.HORIZONTAL_WRAP);
	        list[index].setVisibleRowCount(mirrorArr[index].length); //was*2
	        list[index].addListSelectionListener(this);
	        
	        JScrollPane mirrorPanel = new JScrollPane(list[index], ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER,
	        											ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
	        mirrorPanel.setForeground(Color.black);
	        mirrorPanel.setBorder(MipavUtil.buildTitledBorder("Select a "+title));
	        
	        return mirrorPanel;
	                
	    }
	    
	    private JPanel initNonSymmetricalObjects(int index) {
	        JCheckBox[] noMirrorCheckArr = new JCheckBox[noMirrorArr[index].length];
	        JButton[] noMirrorButtonArr = new JButton[noMirrorArr[index].length];
	        ButtonGroup noMirrorGroup = new ButtonGroup();
	        JPanel noMirrorPanel = new JPanel(new GridLayout(noMirrorButtonArr.length/2 + 1, 2));
	        noMirrorPanel.setForeground(Color.black);
	        noMirrorPanel.setBorder(MipavUtil.buildTitledBorder("Select an object"));
	        GridBagConstraints gbc = new GridBagConstraints();
	        gbc.anchor = GridBagConstraints.NORTHWEST;
	        gbc.fill = GridBagConstraints.HORIZONTAL;
	        
	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.ipadx = 0;
	        for(int i=0; i<noMirrorArr[index].length; i++) {
	            noMirrorCheckArr[i] = new JCheckBox();
	            noMirrorCheckArr[i].setEnabled(false);
	            noMirrorCheckArr[i].setHorizontalAlignment(SwingConstants.RIGHT);
	            
	            noMirrorButtonArr[i] = new JButton(noMirrorArr[index][i]);
	            noMirrorButtonArr[i].setFont(MipavUtil.font12B);
	            noMirrorButtonArr[i].setActionCommand(PlugInMuscleImageDisplay.CHECK_VOI);
	            noMirrorButtonArr[i].addActionListener(muscleFrame);
	            noMirrorGroup.add(noMirrorButtonArr[i]);
	            noMirrorPanel.add(noMirrorCheckArr[i], gbc);
	            noMirrorPanel.add(noMirrorButtonArr[i], gbc);
	            
	            //for(int j=0; j<existingVois.size(); j++) {
	            //    if(((VOI)existingVois.get(j)).getName().equals(noMirrorButtonArr[i].getText())) {
	            //        noMirrorCheckArr[i].setSelected(true);
	            //    }
	            //}
	            //System.out.println(noMirrorButtonArr[i].getText()+" is "+noMirrorZ[i]);
	            //zeroStatus.put(noMirrorButtonArr[i].getText(), noMirrorZ[i]);
	        }
	        
	        return noMirrorPanel;
	    }
	    
	    public void actionPerformed(ActionEvent e) {
	    	System.out.println("Caught 2");
	    	String command = e.getActionCommand();
	        displayChanged = false;
	        if(command.equals(CLEAR)) {
	            //clear all VOIs drawn
	        	muscleFrame.getImageA().unregisterAllVOIs();
	        	muscleFrame.updateImages();
	        } else {
	
	            if (command.equals(OK)) {
	                VOI goodVoi = null; //was checkVOI)_
	                //check that VOI conforms to requirements, returns the VOI being modified/created
	                
	                if ( goodVoi != null ) { 
	                    
	                    //save modified/created VOI to file
	                	muscleFrame.getImageA().unregisterAllVOIs();
	                	muscleFrame.getImageA().registerVOI(goodVoi);
	                    String dir = muscleFrame.getImageA().getImageDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
	                    muscleFrame.saveAllVOIsTo(dir);
	                    
	                    String fileDir = muscleFrame.getImageA().getFileInfo(0).getFileDirectory();
	
	                    MipavUtil.displayInfo(/*objectName*/"test"+" VOI saved in folder\n " + fileDir + PlugInMuscleImageDisplay.VOI_DIR);
	                    
	                    muscleFrame.getImageA().unregisterAllVOIs();
	                    muscleFrame.updateImages();
	                    
	                    //completed = true;
	                    //novelVoiProduced = true; //not necessarily
	                    notifyListeners(OK);
	                    //dispose();
	                } else {
	                    //individual error messages are already displayed
	                }
	            } else if (command.equals(OUTPUT)) {
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
	                MipavUtil.showHelp("19014");
	            } 
	        }
	        
	    }
	    
	
	    
	    public void valueChanged(ListSelectionEvent e) {
	    	//should be process in general, change and compute based on srcImage, do not need to load into component,
	    	//though that's where VOIs should be registered.

	    	if(time != 0) {
		    	JList source = (JList)e.getSource();
		    	Object[] selected = source.getSelectedValues();
		    	String[] selectedString = new String[selected.length];
		    	for(int i=0; i<selected.length; i++) {
		    		selectedString[i] = (String)selected[i]+".xml";
		    	}
		    	for(int i=0; i<list.length; i++) {
		    		if(!source.equals(list[i]) && selectedString.length > 0) {
		    			list[i].clearSelection();
		    		}
		    	}
		    	
		    	//Load VOIs and calculations
		    	loadVOIs(selectedString, lutOn);
		    	muscleFrame.updateImages(true);
		    	time = 0;
	    	} else 
	    		time++;
	    	
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
				for (int listNum = 0; listNum < list.length; listNum++) {	    	
	    			Object[] selected = list[listNum].getSelectedValues();
	    			for(int i=0; i<selected.length; i++) 
	    				subList.add(selected[i]);   
	    			totalList.addAll(subList);
		    	}
	    		itr = totalList.iterator();
			}
				
			
			while(itr.hasNext()) {
				Object itrObj = itr.next();
				double totalAreaCalc = 0, totalAreaCount = 0, fatArea = 0, leanArea = 0;//, partialArea = 0;
				double meanFatH = 0, meanLeanH = 0, meanTotalH = 0;
				//pixels -> cm^2
				totalAreaCalc = (Double)totalAreaCalcTree.get(itrObj);
				totalAreaCount = (Double)totalAreaCountTree.get(itrObj);
				fatArea = (Double)fatAreaTree.get(itrObj);
				leanArea = (Double)leanAreaTree.get(itrObj);
				meanFatH = (Double)meanFatHTree.get(itrObj);
				meanLeanH = (Double)meanLeanHTree.get(itrObj);
				meanTotalH = (Double)meanTotalHTree.get(itrObj);
				
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
		
			if (doSave) {
					    		
				//now load all VOIs at once:
				int totalSize = 0;
				for (int listNum = 0; listNum < list.length; listNum++) {
		    		ListModel model = list[listNum].getModel();	
		    		totalSize += model.getSize();
				}
				
				String [] allStrings = new String[totalSize];
				
				int counter = 0;
				for (int listNum = 0; listNum < list.length; listNum++) {
		    		ListModel model = list[listNum].getModel();		    	
		    		String [] listStrings = new String[model.getSize()];
		    			
		    		
		    		for (int i = 0; i < listStrings.length; i++, counter++) {
		    			allStrings[counter] = (String)model.getElementAt(i) + ".xml";
		    		}
				} 
				loadVOIs(allStrings, true);
			
				//loadLUT();
				getActiveImage().getParentFrame().updateImages(true);
				updateImages(true);
				
				
				java.awt.Image edgeImage = captureImage();
				
				loadVOIs(new String[] {}, false);
				loadLUT();
				java.awt.Image qaImage = captureImage();
				removeLUT();
				closePDF(edgeImage, qaImage);
				getActiveImage().getParentFrame().updateImages(true);
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
	    	Thread calc = new Thread(muscleCalc);
	    	calc.start();
	    }
	    
		/**
		 * Performs required calculations for plugin. Error estimated at 2%.
		 * 
		 * @author senseneyj
		 *
		 */
	    private class MuscleCalculation implements Runnable {
	    	private boolean done = false;
	    	
	    	public void run() {
	    		long time = System.currentTimeMillis();
	    		loadVOIs(totalList, false);
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
    
    public void loadVOIs(String[] voiName, boolean fillVOIs) {
        getActiveImage().unregisterAllVOIs();
        int colorChoice = new Random().nextInt(colorPick.length);
        String fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
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
                    	Color c = hasColor(voiVec[0]);
                        if(c != null) {
                            voiVec[0].setColor(c);
                            
                        } else {
                            voiVec[0].setColor(colorPick[colorChoice % colorPick.length]);
                            colorChoice++;
                        }                      
                        voiVec[0].setThickness(2);
                        if(fillVOIs && getZeroStatus(voiVec[0].getName())) {
                        	voiVec[0].setDisplayMode(VOI.SOLID);
                        	voiVec[0].setOpacity((float)0.7);
                        }
                        getActiveImage().registerVOI(voiVec[0]);
                    }
                }
            }
        }  
    }
    
    private Color hasColor(VOI voiVec) {
        Color c = null;
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
    
    public VOI getSingleVOI(String name) {
    	if(getActiveImage() == null) {
    		int j = 1;
    	}
        String fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
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
	    
	    private int i;
	    
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
	            if (smoothAlgo[i].isCompleted() == true && thighCompleted[i]) {
	
	                // The algorithm has completed and produced a
	                resultVOI = smoothAlgo[i].getResultVOI();
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
	    SwingUtilities.convertPointToScreen(p, getActiveImage().getParentFrame());//.getContentPane());
	    p.x += 356; // correcting for side panel TODO: find lengths
	    p.y += (42-7); // correcting for titled border
	
	    Dimension d = new Dimension();
	    d.width = getActiveImage().getFileInfo()[0].getExtents()[0];//.getParentFrame().getContentPane().getWidth() - 3; // the -3 is a correction
	    d.height = getActiveImage().getFileInfo()[0].getExtents()[1];//getActiveImage().getParentFrame().getContentPane().getHeight() - 3; // ""
	    currentRectangle = new Rectangle(p, d); //p, d
	    
	    try {
	        Robot robot = new Robot();
	
	        return robot.createScreenCapture(currentRectangle);
	    } catch (OutOfMemoryError error) {
	    } catch (AWTException error) {
	    }
	    return null;
	}

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
			
			
			
			
		//	imageTable.addCell(Image.getInstance(display.get));
			
		//	imageTable.addCell("QA Image");
			
		//	chooser = new JFileChooser();
		//	chooser.setDialogTitle("Open image 1");
		//	returnVal = chooser.showOpenDialog(null);
	
	   //     while (returnVal != JFileChooser.APPROVE_OPTION) {
	   //     	returnVal = chooser.showOpenDialog(null);
	   //     } 
			
		//	imageTable.addCell(Image.getInstance(chooser.getSelectedFile().getPath()));
			
		//	chooser.setDialogTitle("Open image 1");
		//	returnVal = chooser.showOpenDialog(null);
	
	   //     while (returnVal != JFileChooser.APPROVE_OPTION) {
	   //     	returnVal = chooser.showOpenDialog(null);
	   //     } 
			
	   //     imageTable.addCell(Image.getInstance(chooser.getSelectedFile().getPath()));
	        
	   //     Paragraph pImage = new Paragraph();
	   //     pImage.add(new Paragraph());
	   //     pImage.add(imageTable);
	        
		//	pdfDocument.add(pImage);
			
			} catch (Exception e) {
				e.printStackTrace();
			}
			
		}

	protected void createPDF() {		
		String fileDir = getActiveImage().getFileInfo(0).getFileDirectory();
		long time = System.currentTimeMillis();
		
		File file = new File(fileDir + File.separator + "NIA_Seg-" + time + ".pdf");
		System.err.println("file: " + file.toString());
		try {
			pdfDocument = new Document();
			writer = PdfWriter.getInstance(pdfDocument, new FileOutputStream(file));
			pdfDocument.addTitle("Thigh Tissue Analysis Report");
			pdfDocument.addCreator("MIPAV: Muscle Segmentation");
			pdfDocument.open();
			
			
			Paragraph p = new Paragraph();
			
			//add the Title and subtitle
			p.setAlignment(Element.ALIGN_CENTER);
			p.add(new Chunk("MIPAV: Segmentation", new Font(Font.TIMES_ROMAN, 18)));
			p.add(new Paragraph());
			p.add(new Chunk("Thigh Tissue Analysis Report", new Font(Font.TIMES_ROMAN, 12)));
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
			mct2.addElement(new Paragraph(getActiveImage().getFileInfo()[0].getFileName(), fontNormal));
			mct2.addElement(new Paragraph("Center:", fontBold));
			mct2.addElement(new Paragraph("Hjartavernd", fontNormal));
			FileInfoBase[] f = getActiveImage().getFileInfo();
			pdfDocument.add(mct2);
			
			MultiColumnText mct3 = new MultiColumnText(20);
			mct3.setAlignment(Element.ALIGN_LEFT);
			mct3.addRegularColumns(pdfDocument.left(), pdfDocument.right(), 10f, 4);
			mct3.addElement(new Paragraph("ID:", fontBold));
			mct3.addElement(new Paragraph(getActiveImage().getFileInfo()[0].getFileName(), fontNormal));
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
			spTable.addCell(Float.toString(getActiveImage().getFileInfo()[0].getSliceThickness()));
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
	 * Adds a row to the PDF Table
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
			DecimalFormat dec = new DecimalFormat("0.#");
			
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
}
