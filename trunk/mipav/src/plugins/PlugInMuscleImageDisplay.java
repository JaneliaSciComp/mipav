import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmSnake;
import gov.nih.mipav.model.algorithms.AlgorithmVOIProps;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.ScriptRecorder;
import gov.nih.mipav.model.scripting.actions.ActionCloseFrame;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics;
import gov.nih.mipav.view.dialogs.JDialogVOIStats;
import gov.nih.mipav.view.dialogs.JDialogWinLevel;

import com.lowagie.text.*;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.pdf.*;

import java.awt.*;
import java.awt.Rectangle;
import java.awt.event.*;
import java.io.*;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.*;

import javax.swing.*;
import javax.swing.border.EmptyBorder;

/**
 * 
 * 
 * @author Justin Senseney (senseneyj@mail.nih.gov)
 *
 */
public class PlugInMuscleImageDisplay extends ViewJFrameImage implements AlgorithmInterface {
    
    //~ Static fields --------------------------------------------------------------------------------------------------
    
    /** Available colors.*/
	public static final Color[] colorPick = {Color.GREEN, Color.ORANGE, Color.CYAN, 
                                                Color.YELLOW, Color.MAGENTA, Color.RED};
    
    /**Folder for saving all information to.*/
    public static final String VOI_DIR = "NIA_Seg";
    
    public static final String TOTAL_AREA = "Total Area";   
    public static final String FAT_AREA = "Fat Area";   
    public static final String LEAN_AREA = "Lean Area";  
    public static final String MEAN_TOTAL_HU = "Mean Total HU";  
    public static final String MEAN_FAT_HU = "Mean Fat HU";
    public static final String MEAN_LEAN_HU = "Mean Lean HU";
    
    //~ Instance fields ------------------------------------------------------------------------------------------------    
    

    /**For writing PDF docs below. */
    private Document pdfDocument = null;
	private PdfWriter pdfWriter = null;
	private File pdfFile = null;
	private File textFile = null;
	private PdfPTable wholeTable = null;
	private PdfPTable[] sliceTable = null;
	private PdfPTable imageTable = null;
    
    /** Location of the VOI tab. */
    private int voiTabLoc;
    
    /** Location of the analysis tab. */
    private int resultTabLoc;
    
    /** Text for muscles where a mirror muscle may exist. */
    private String[][] mirrorArr;

    /** Text for muscles where mirror muscles are not considered. */
    private String[][] noMirrorArr;

    /**Denotes the anatomical part represented in the image. Implemented here in case 
     * this class is moved to its own class at a later time. */
    private ImageType imageType;

    /** Whether this image has mirror image muscles (eg VIEWS of thighs, abdomen. */
    private Symmetry symmetry;
    
    /**The dialog tabs that exist in this frame.*/
    private JTabbedPane dialogTabs;
    
    /**Currently open muscle tab, used to return from VOI tabs or Analysis tabs.*/
    private int activeTab;
    
    /**All tabs within this plugin. */
    private DialogPrompt[] tabs;
    
    /**The current slice of a 3D image, should be same as getViewableSlice().*/
    private int currentSlice = 0;
    
    /**Whether the algorithm is dealing with a 3D CT image. */
    private boolean multipleSlices;
    
    /**The titles of all tabs. */
    private String[] titles; 
    
    /**The directory of the current image.*/
    private String imageDir;
    
    /**Whether the previously called VOI was modified and needs to be saved to the file system.*/
    private boolean voiChangeState = false;

    /**The seed point for choosing a color. */
    private int colorChoice = 0;
    
    /**Buffer containing exact copies of VOIs on file system along with program relevant material.*/
    private Map<String, PlugInSelectableVOI> voiBuffer;
    
    /**The top-level group of threads used for calculating. */
    private ThreadGroup calcGroup = new ThreadGroup("CalcVOI");
    
    /**The algorithm for producing bones automatically. */
    private PlugInAlgorithmCTBone boneSeg;
    
    /**The algorithm for producing marrow automatically. */
    private PlugInAlgorithmCTMarrow marrowSeg;
    
    /**The algorithm for producing a thighs automatically. */
    private PlugInAlgorithmCTThigh thighSeg;
    
    /**The algorithm for producing an abdomen automatically. */
    private PlugInAlgorithmCTAbdomen abdomenSeg;
    
    /**A map indicating whether a VOI of a particular name should be calculated. */
    private TreeMap<String, Boolean> calcTree;
     
    /**User specified preference for whether to ask on closing. */
    private boolean oldPrefCloseFrameCheckValue = Preferences.is(Preferences.PREF_CLOSE_FRAME_CHECK);
    
    /**Frame of original image, hidden until plugin is exited.*/
    //private Frame hiddenFrame;
    
    /** Whether the algorithm is being run in standAlone mode.*/
    private boolean standAlone;
    
    /**All MuscleCalculations currently in progress */
    private ArrayList<MuscleCalculation> muscleCalcList;
    
    public enum ImageType{
        
        /** denotes that the srcImg is an abdomen */
        Abdomen,
        
        /** denotes that the srcImg is two thighs */
        Thigh,
        
        /** custom means a user defined body type has been created or loaded*/
        Custom, 
        
        /** unknown image type, generally represents an error state */
        Unknown,
        
        /** 
         * ImageType is defined at run time, note that maintaing this distinction allows for
         * dependencies to be integrated, will see if this funcionality is needed.
         */
        RunTimeDefined
    }
    
    public enum Symmetry {
        
        /** Indicates the image has no symmetry. */
        NO_SYMMETRY("No symmetry", ""), 
        
        /** Indicates that image has left-right symmetry. */
        LEFT_RIGHT("Left", "Right"), 
        
        /** Indicates the image has top-bottom symmetry. */
        TOP_BOTTOM("Top", "Bottom");
        
        final String side1;
        final String side2;
        
        Symmetry(String side1, String side2) {
        	this.side1 = side1;
        	this.side2 = side2;
        }
        
        public String toString() {
        	if(side2.length() == 0) {
        		return side1;
        	}
        	return side1+"/"+side2;
        }
    }
 
    /**
     * Constructor for creating plugin within MIPAV system.
     * @param image
     * @param titles
     * @param voiList
     * @param imageType
     * @param symmetry
     * @param multipleSlices
     */
    public PlugInMuscleImageDisplay(ModelImage image, String[] titles,
            PlugInSelectableVOI[][] voiList,  
            ImageType imageType, Symmetry symmetry, boolean multipleSlices) {

    	super(image);

    	setVisible(false);
     
        commonConstructor(image, titles, voiList,  imageType, symmetry, false, multipleSlices);

        File f;
        if(!(f = new File(imageDir+File.separator)).exists()) {
        	f.mkdir();
        }

        createVOIBuffer();
        
        initNext();
        
        commonInitializer(voiList);
    }
    
    /**
     * Constructor for calling this as a stand-alone.  will invoke the class's own init() function
     * for setting up the image rather than ViewJFrameImage's init()
     * @param image the model image
     * @param titles
     * @param mirrorCalcItemsArr
     * @param mirrorZ
     * @param noMirrorCalcItemsArr
     * @param noMirrorZ
     * @param imageType
     * @param symmetry
     * @param standAlone
     */
    public PlugInMuscleImageDisplay(ModelImage image, String[] titles,
            PlugInSelectableVOI[][] voiList, 
            ImageType imageType, Symmetry symmetry, 
            boolean standAlone, boolean multipleSlices) {
    	
    	// calls the ViewJFrameBase constructor that will not call ViewJFrameImage's init() function
    	super(image, (ModelImage)null);
    	
    	
    	
    	commonConstructor(image, titles, voiList,  imageType, symmetry, standAlone, multipleSlices);

    	setVisible(false);
    	progressBar.setVisible(true);

        createVOIBuffer();

    	initStandAlone();
    	setVisible(false);
    	this.setActiveImage(IMAGE_A);
    	scrollPane.requestFocus();
    	
    	ctMode(getImageA(), -175, 275);

    	if (imageA == null) {
            return;
        }
        progressBar.updateValue(5);
        
        File f;
        if(!(f = new File(imageDir+File.separator)).exists()) {
        	f.mkdir();
        }

        ViewUserInterface.getReference().getMessageFrame().append("Exists? "+f.exists()+"\n", ViewJFrameMessage.DEBUG);
        createVOIBuffer();
        
        commonInitializer(voiList);
    }
    
    private void commonConstructor(ModelImage image, String[] titles,
	        PlugInSelectableVOI[][] voiList,  
	        ImageType imageType, Symmetry symmetry, boolean standAlone, boolean multipleSlices) {
		this.setImageA(image); 
	    this.titles = titles; 
	    this.mirrorArr = new String[voiList.length][]; 
	    this.noMirrorArr = new String[voiList.length][]; 
	    this.calcTree = new TreeMap<String, Boolean>(); 
	    this.voiBuffer = Collections.synchronizedMap(new TreeMap<String, PlugInSelectableVOI>()); 
	    this.standAlone = standAlone;
	    this.imageType = imageType; 
	    this.symmetry = symmetry; 
	    this.multipleSlices = multipleSlices; 
	    this.currentSlice = getViewableSlice();
	    this.colorChoice = 0;
	    //left as zero to ensure VOIs across image stay same color (helps for image batches)
	
	    for(int i=0; i<voiList.length; i++) {
	    	ArrayList<Comparable> mirrorArrList = new ArrayList<Comparable>(), noMirrorArrList = new ArrayList<Comparable>(), 
	    				mirrorZList = new ArrayList<Comparable>(), noMirrorZList = new ArrayList<Comparable>();
	    	for(int j=0; j<voiList[i].length; j++) {
	    		voiBuffer.put(voiList[i][j].getName(), voiList[i][j]);
	    		if(voiList[i][j].getName().contains("Left")) {
	    			mirrorArrList.add(voiList[i][j].getName().substring(new String("Left ").length()));
	    			mirrorZList.add(voiList[i][j].getFillEligible());
	    			calcTree.put(voiList[i][j].getName().substring(new String("Left ").length()), voiList[i][j].getCalcEligible());
	    		} else if(voiList[i][j].getName().contains("Right")) {
	    			//do nothing
	    		} else {
	    			noMirrorArrList.add(voiList[i][j].getName());
	    			noMirrorZList.add(voiList[i][j].getFillEligible());
	    			calcTree.put(voiList[i][j].getName(), voiList[i][j].getCalcEligible());
	    		}
	    	}
	    	mirrorArr[i] = new String[mirrorArrList.size()];
	    	for(int j=0; j<mirrorArr[i].length; j++) {
	    		mirrorArr[i][j] = (String)mirrorArrList.get(j);
	    	}
	    	noMirrorArr[i] = new String[noMirrorArrList.size()];
	    	for(int j=0; j<noMirrorArr[i].length; j++) { 
	    		noMirrorArr[i][j] = (String)noMirrorArrList.get(j);
	    	}
	    }
	    
	    Preferences.setProperty(Preferences.PREF_CLOSE_FRAME_CHECK, String.valueOf(true));
	    
	    progressBar = new ViewJProgressBar("Automatic Seg", "Initializing...", 0, 100, true);
	    if (imageA == null) {
	        return;
	    }
	    progressBar.updateValue(5);
	    imageDir = getImageA().getFileInfo(getViewableSlice()).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
	    
	    //Propagate children relationship backwards
	    setDependents();
	}

	private void commonInitializer(PlugInSelectableVOI[][] voiList) {
		//store musclecalculations for tracking
		muscleCalcList = new ArrayList<MuscleCalculation>();
		
		//Automatic segmentation here
	    progressBar.setMessage("Creating algorithms...");
	    progressBar.setSeparateThread(true);
	    long time = System.currentTimeMillis();
	    progressBar.updateValue(10);
	    autoSegmentation();
	    progressBar.setVisible(true);
	    progressBar.updateValue(20);
		progressBar.setMessage("Segmenting...");
	    System.err.println("Time spent creating algs: "+(System.currentTimeMillis() - time));
	    time = System.currentTimeMillis();
	    waitAlg(progressBar);
	    System.err.println("Total time spent waiting for threads: "+(System.currentTimeMillis() - time));
	    progressBar.updateValue(90);
		progressBar.setMessage("Displaying results...");
		for(int i=voiList.length; i>0; i--) {
			getActiveImage().unregisterAllVOIs();
			initMuscleImage(i-1);
	    }
	    progressBar.setVisible(false);
	    progressBar.dispose();
	    setVisible(true);
	    
	    forceVoiRecalc();
	}
	
	public void forceVoiRecalc() {
		
		long time = System.currentTimeMillis();
		ViewJProgressBar interruptStatus = new ViewJProgressBar("Interrupting", "Restarting calculations...", 0, 100, false);
		
		if(!muscleCalcList.isEmpty()) {
			interruptStatus.setVisible(true);
		}
		
		int index = 0;
		while(!muscleCalcList.isEmpty()) {
			index = muscleCalcList.size()-1;
			if(index > -1) {
				muscleCalcList.get(index).interrupt();
			} 
		}
		
		interruptStatus.setVisible(false);
		
		System.err.println("Finished interrupting in "+(System.currentTimeMillis() - time));
		
		//end automatic segmentation, prepare buffer for automatic calculations
	    Iterator<String> itr = voiBuffer.keySet().iterator();
	    
	    while(itr.hasNext()) {
	    	PlugInSelectableVOI voi = voiBuffer.get(itr.next());
	    	if(voi.getCalcEligible() && !voi.isEmpty()) {
	    		long timeVOI = System.currentTimeMillis();
		        voi.setLastModified(timeVOI);
	    	}
	    }
	    
	    //always perform new calculation here, no need to check, kill any previous calculations
	    itr = voiBuffer.keySet().iterator();
	    while(itr.hasNext()) {
	    	PlugInSelectableVOI voi = voiBuffer.get(itr.next());
	    	if(voi.getCalcEligible() && !voi.isEmpty()) {
		        MuscleCalculation muscleCalc = new MuscleCalculation(voi, voi.getName());
		        Thread calc = new Thread(calcGroup, muscleCalc, voi.getName());
		        calc.start();
	    	}
	    }
	}

    public void actionPerformed(ActionEvent e) {
    	ViewUserInterface.getReference().getMessageFrame().append("An action: "+e+"\n", ViewJFrameMessage.DEBUG);
    	String command = e.getActionCommand();
        //run through toggle buttons to see if a menu selected one (updates the button status)
        getControls().getTools().setToggleButtonSelected(command);
                
        if(command.equals(DialogPrompt.CHECK_VOI)) {
            ((VoiDialogPrompt)tabs[voiTabLoc]).setUpDialog(((JButton)(e.getSource())).getText());
            lockToPanel(voiTabLoc, "VOI"); //includes making visible
            initVoiImage(); //replacing current image and updating
        } else if(command.equals(DialogPrompt.CALCULATE)) {
        	lockToPanel(resultTabLoc, "Analysis"); //includes making visible
        	if(imageType.equals(ImageType.Abdomen) && voiBuffer.get("Liver").getTotalArea() != 0) {
        		MuscleCalculation muscleCalc = new MuscleCalculation(voiBuffer.get("Liver"), "Liver");
                Thread calc = new Thread(calcGroup, muscleCalc, "Liver");
                calc.start();
        	}
        	getActiveImage().unregisterAllVOIs();
	    	updateImages(true);
	    	boolean leftMarrow = false, rightMarrow = false, rightBone = false, leftBone = false, abdomen = false;
        	if((imageType.equals(ImageType.Thigh) && ((leftMarrow = !voiBuffer.get("Left Marrow").getCreated()) || 
        												(rightMarrow = !voiBuffer.get("Right Marrow").getCreated()) || 
        												(rightBone = !voiBuffer.get("Right Bone").getCreated()) || 
        												(leftBone = !voiBuffer.get("Left Bone").getCreated()))) || 
				(imageType.equals(ImageType.Abdomen) && ((abdomen = !voiBuffer.get("Abdomen").getCreated())))) {
        		String createStr = new String();
        		if(abdomen) {
        			createStr += "\tAbdomen\n";
        		}
        		if(leftMarrow) {
        			createStr += "\tLeft Marrow\n";
        		}
        		if(rightMarrow) {
        			createStr += "\tRight Marrow\n";
        		}
        		if(leftBone) {
        			createStr += "\tLeft Bone\n";
        		}
        		if(rightBone) {
        			createStr += "\tRightBone\n";
        		}
        		MipavUtil.displayWarning("This tab calculates VOIs that depend on the following being created.\n"+
        				"Note that only muscle calculations will be correct.\n"+createStr);
        	}
        	((AnalysisDialogPrompt)tabs[resultTabLoc]).setSlice(getViewableSlice());
        	((AnalysisDialogPrompt)tabs[resultTabLoc]).setUpDialog();
        	((AnalysisDialogPrompt)tabs[resultTabLoc]).enableCalcOutput();
        } else if(command.equals(DialogPrompt.CANCEL)) {
    		unlockToPanel(voiTabLoc);
    		//initMuscleImage(activeTab);
    	} else if(command.equals(DialogPrompt.HIDE_ALL)) {
    		getActiveImage().unregisterAllVOIs();
    		updateImages(true);
    	} else if(command.equals(DialogPrompt.OK) && 
    			tabs[voiTabLoc].getCompleted() == true) {
    		unlockToPanel(voiTabLoc);
    		//initMuscleImage(activeTab);
    	} else if(command.equals(DialogPrompt.BACK)) {
    		unlockToPanel(resultTabLoc);
    		getActiveImage().unregisterAllVOIs();
    		//initMuscleImage(activeTab);
    	} else if(command.equals(DialogPrompt.EXIT)) {
        	close();
    	} else if(command.equals(DialogPrompt.HELP)) {
    		if(imageType.equals(ImageType.Thigh)) {
    			MipavUtil.showHelp("MS00001");
    		} else { //image is of type abdomen
    			MipavUtil.showHelp("MS00050");
    		}
    	} else {
    		super.actionPerformed(e);
    	}
    }
    
    /**
	 * Handles automatic segmentation algorithm completions.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
			
		Vector<VOIBase> firstVOI = null;
		Vector<VOIBase> secondVOI = null;
		PlugInSelectableVOI firstBufferVOI = null;
		PlugInSelectableVOI secondBufferVOI = null;
		if(algorithm instanceof PlugInAlgorithmCTThigh) {
			if(((PlugInAlgorithmCTThigh)algorithm).getLeftThighVOI() != null && 
					((PlugInAlgorithmCTThigh)algorithm).getRightThighVOI() != null) {
				ViewUserInterface.getReference().getMessageFrame().append("Thigh VOIs completed correctly\n", ViewJFrameMessage.DEBUG);
				firstVOI = ((PlugInAlgorithmCTThigh)algorithm).getLeftThighVOI().getCurves();
				secondVOI = ((PlugInAlgorithmCTThigh)algorithm).getRightThighVOI().getCurves();
				firstBufferVOI = voiBuffer.get("Left Thigh");
				secondBufferVOI = voiBuffer.get("Right Thigh");
				for(int j=0; j<firstVOI.size(); j++) {
				    firstBufferVOI.importCurve((VOIContour)firstVOI.get(j));
				}
				for(int j=0; j<secondVOI.size(); j++) {
				    secondBufferVOI.importCurve((VOIContour)secondVOI.get(j));
				}
				firstBufferVOI.setComputerGenerated(true);
				secondBufferVOI.setComputerGenerated(true);
				firstBufferVOI.setSegmentationTime(((PlugInAlgorithmCTThigh)algorithm).getSegmentationTimeThigh());
				secondBufferVOI.setSegmentationTime(((PlugInAlgorithmCTThigh)algorithm).getSegmentationTimeThigh());
			}
		} else if(algorithm instanceof PlugInAlgorithmCTMarrow) {
			if(((PlugInAlgorithmCTMarrow)algorithm).getLeftMarrowVOI() != null && 
					((PlugInAlgorithmCTMarrow)algorithm).getRightMarrowVOI() != null) {
				ViewUserInterface.getReference().getMessageFrame().append("Marrow VOIs completed correctly\n", ViewJFrameMessage.DEBUG);
				firstVOI = ((PlugInAlgorithmCTMarrow)algorithm).getLeftMarrowVOI().getCurves();
				secondVOI = ((PlugInAlgorithmCTMarrow)algorithm).getRightMarrowVOI().getCurves();
				firstBufferVOI = voiBuffer.get("Left Marrow");
				secondBufferVOI = voiBuffer.get("Right Marrow");
				for(int j=0; j<firstVOI.size(); j++) {
				    firstBufferVOI.importCurve((VOIContour)firstVOI.get(j));
				}
				for(int j=0; j<secondVOI.size(); j++) {
				    secondBufferVOI.importCurve((VOIContour)secondVOI.get(j));
				}
				firstBufferVOI.setComputerGenerated(true);
				secondBufferVOI.setComputerGenerated(true);
				firstBufferVOI.setSegmentationTime(((PlugInAlgorithmCTMarrow)algorithm).getSegmentationTimeMarrow());
				secondBufferVOI.setSegmentationTime(((PlugInAlgorithmCTMarrow)algorithm).getSegmentationTimeMarrow());
			}
		} else if(algorithm instanceof PlugInAlgorithmCTBone) {
			if(((PlugInAlgorithmCTBone)algorithm).getLeftBoneVOI() != null && 
					((PlugInAlgorithmCTBone)algorithm).getRightBoneVOI() != null) {
				ViewUserInterface.getReference().getMessageFrame().append("Bone VOIs completed correctly\n", ViewJFrameMessage.DEBUG);
				firstVOI = ((PlugInAlgorithmCTBone)algorithm).getLeftBoneVOI().getCurves();
				secondVOI = ((PlugInAlgorithmCTBone)algorithm).getRightBoneVOI().getCurves();
				firstBufferVOI = voiBuffer.get("Left Bone");
				secondBufferVOI = voiBuffer.get("Right Bone");
				for(int j=0; j<firstVOI.size(); j++) {
				    firstBufferVOI.importCurve((VOIContour)firstVOI.get(j));
				}
				for(int j=0; j<secondVOI.size(); j++) {
				    secondBufferVOI.importCurve((VOIContour)secondVOI.get(j));
				}
				firstBufferVOI.setComputerGenerated(true);
				secondBufferVOI.setComputerGenerated(true);
				firstBufferVOI.setSegmentationTime(((PlugInAlgorithmCTBone)algorithm).getSegmentationTimeBone());
				secondBufferVOI.setSegmentationTime(((PlugInAlgorithmCTBone)algorithm).getSegmentationTimeBone());
			}
		} else if(algorithm instanceof PlugInAlgorithmCTAbdomen) {
			if(((PlugInAlgorithmCTAbdomen)algorithm).getAbdomenVOI() != null) {
				ViewUserInterface.getReference().getMessageFrame().append("Abdomen alg completed\n", ViewJFrameMessage.DEBUG);
				firstVOI = ((PlugInAlgorithmCTAbdomen)algorithm).getAbdomenVOI().getCurves();
				firstBufferVOI = voiBuffer.get("Abdomen");
				for(int j=0; j<firstVOI.size(); j++) {
				    firstBufferVOI.importCurve((VOIContour)firstVOI.get(j));
				}
				firstBufferVOI.setComputerGenerated(true);
				firstBufferVOI.setSegmentationTime(((PlugInAlgorithmCTAbdomen)algorithm).getSegmentationTimeAbd());
			}
			if(((PlugInAlgorithmCTAbdomen)algorithm).getSubcutaneousVOI() != null) {
				secondVOI = ((PlugInAlgorithmCTAbdomen)algorithm).getSubcutaneousVOI().getCurves();
				secondBufferVOI = voiBuffer.get("Subcutaneous area");
				for(int j=0; j<secondVOI.size(); j++) {
				    secondBufferVOI.importCurve((VOIContour)secondVOI.get(j));
				}
				secondBufferVOI.setComputerGenerated(true);
				secondBufferVOI.setSegmentationTime(((PlugInAlgorithmCTAbdomen)algorithm).getSegmentationTimeSub());
			}
		}
	
	    if (algorithm != null) {
	        algorithm.finalize();
	        algorithm = null;
	    }
	    if(standAlone) {
	    	initControls();
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
            } else if(reply == JOptionPane.YES_OPTION && !standAlone) {
            	//reshow the hidden frame
            	//hiddenFrame.setVisible(true);
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
    
	public void componentShown(ComponentEvent event) {
    	Component c = event.getComponent();
	    if(c instanceof MuscleDialogPrompt) {
	    	for(int i=0; i<voiTabLoc; i++) {
	    		if(tabs[i].equals(c) && !voiChangeState) {
	    			getActiveImage().unregisterAllVOIs();
	    			initMuscleImage(i);
	    			activeTab = i;
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
        	
            setSize(width, height);
            validate();
            setTitle();
           
            updateImages(true);
        }
        
        addComponentListener(this);  
    }
    
    /**
	 * Easy curveExists for checks if curveExists for viewableSlice(). (so no bounds check)
	 */
	public boolean curveExists(String name) {
		if(voiBuffer.get(name).getSliceSize(getViewableSlice()) > 0) {
			return true;
		}
		return false;
	}

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
		
		dialogTabs.removeAll();
		dialogTabs = null;
	
		System.gc();
		
		if (ViewUserInterface.getReference().isAppFrameVisible() == false &&
				ViewUserInterface.getReference().getActiveImageFrame() == null) {
			System.exit(0);
		}
		
	}

	/**
     * Gets whether calculations are still being performed, vital for whether reliable statistics are available
     * @return null if all calcs done
     */
    public String[] getCalcRunning() {
    	if(calcGroup.activeCount() != 0) {
    		Thread[] activeThreads = new Thread[calcGroup.activeCount()];
    		ArrayList<String> threadStr = new ArrayList<String>();
    		calcGroup.enumerate(activeThreads);
    		for(int i=0; i<activeThreads.length; i++) {
    			if(activeThreads[i] != null) {
    				threadStr.add(activeThreads[i].getName());
    			}
    		}
    		String[] strArr = new String[threadStr.size()]; 
    		for(int i=0; i<threadStr.size(); i++) {
    			strArr[i] = threadStr.get(i);
    		}
    		return strArr;
    	} 
    		
    	return null;
    }  
    
    /**
	 * Identifies which panel a particular VOI belongs to
	 * 
	 * @param name the VOI
	 * @return a panel where 0 is first
	 */
	public int getLocationStatus(String name) {
		int loc = PlugInSelectableVOI.INVALID_LOC_NUMBER;
		if(voiBuffer.get(name) != null) {
			loc = voiBuffer.get(name).getLocation();
		}
		return loc;
	}

	/**
	 * Gets all VOIs of given names from the buffer that have been created
	 */
	
	public void getVOIs(String[] voiName, double fillVOIs) {
		getActiveImage().unregisterAllVOIs();
		ArrayList<String> newName = new ArrayList<String>();
		String v;
		for(int i=0; i<voiName.length; i++) {
			if(voiBuffer.get(v = voiName[i].substring(0, voiName[i].lastIndexOf('.'))).getCreated()) {
				newName.add(v);
			}
		}
		
		VOI tempVOI;
		for(int i=0; i<newName.size(); i++) {
			getActiveImage().registerVOI(tempVOI = voiBuffer.get(newName.get(i)));
			if(fillVOIs != 0 && getZeroStatus(newName.get(i))) {
	        	tempVOI.setDisplayMode(VOI.SOLID);
	        	tempVOI.setOpacity((float)fillVOIs);
	        }
		}
	}

	/**
	 * Identifies whether a VOI should be filled in
	 * 
	 * @param name the VOI
	 * @return true, VOI to be filled
	 */
	public boolean getZeroStatus(String name) {
		boolean fill = false;
		if(voiBuffer.get(name) != null) {
			fill = voiBuffer.get(name).getFillEligible();
		}
		return fill;
	}

	/**
	 * Loads VOI and automatically stores it into the voiBuffer.
	 * @param name
	 * @return
	 */
	public PlugInSelectableVOI loadVOI(String name) {
		String fileDir;
		fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+File.separator;
		String ext = name.contains(".xml") ? "" : ".xml";
		PlugInSelectableVOI temp = voiBuffer.get(name);
		temp.getCurves().removeAllElements();
		
	    if(new File(fileDir+name+ext).exists()) {
	        FileVOI v;
	        VOI[] voiVec = null;
	        try {
	            v = new FileVOI(name+ext, fileDir, getActiveImage());
	            voiVec = v.readVOI(false);
	        } catch(IOException e) {
	            MipavUtil.displayError("Unable to load old VOI from location:\n"+fileDir+"\nWith name: "+name);
	            e.printStackTrace();
	        }
	        if(voiVec.length > 1) {
	        	//though VOI could contain multiple curves, no file loaded should contain multiple VOIs
	            MipavUtil.displayError("Invalid VOI from location:\n"+fileDir+"\nWith name: "+name);
	        } else {
	            Vector <VOIBase> voiVecTemp = voiVec[0].getCurves();
	            for(int j=0; j<voiVecTemp.size(); j++) { 
	                temp.importCurve((VOIContour)(voiVecTemp.get(j)));
	            }
	        }
	    }
	    return temp;
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
	    fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+File.separator;
	    File allVOIs = new File(fileDir);
	    if(allVOIs.isDirectory()) {
	        for(int i=0; i<voiName.length; i++) {
	            //ViewUserInterface.getReference().getMessageFrame().append(voiName[i]);
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
	                	if(voiBuffer.get(voiVec[0].getName()).getColor().equals(PlugInSelectableVOI.INVALID_COLOR)) {
	                    	Color c = hasColor(voiVec[0]);
	                        if(!(c = hasColor(voiVec[0])).equals(PlugInSelectableVOI.INVALID_COLOR)) {
	                            voiVec[0].setColor(c);
	                        } else {
	                            voiVec[0].setColor(c = colorPick[colorChoice++ % colorPick.length]);
	                        }
	                        voiBuffer.get(voiVec[0].getName()).setColor(c);
	                	} else {
	                		voiVec[0].setColor(voiBuffer.get(voiVec[0].getName()).getColor());
	                	}
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

	/**
	 * Lock the dialog to a specific panel, such as VOI or Analysis
	 * 
	 * @param tabLoc the tab to open
	 * @param title the title of the tab to open
	 */
	public void lockToPanel(int tabLoc, String title) {
		tabs[tabLoc].setVisible(true);
		for(int i=0; i<voiTabLoc; i++) {
			dialogTabs.setEnabledAt(i, false);
		}
		dialogTabs.addTab(title, tabs[tabLoc]);
		dialogTabs.setSelectedIndex(voiTabLoc);
		updateImages(true);
	}

	/**
	 * Whether a particular VOI exists
	 * @param name of the VOI to search for
	 * @return
	 */
	public boolean voiExists(String name) {
		return voiBuffer.get(name).getCreated();
	}

	/**
	 * Whether a VOI contains a particular curve for the given slice.  If bad name or slice number, 
	 * should return false and perform bounds check, but does not currently
	 * 
	 * @param name Name of VOI to check for
	 * @param slice Slice number to check
	 * @return
	 */
	public boolean voiExists(String name, int slice) {
		if(!multipleSlices) {
			return voiExists(name);
		}
		if(voiBuffer.get(name).getSliceSize(slice) > 0) {
			return true;
		}
		return false;
	}

	/**
	 * This method saves all VOIs for the active image to a given directory.  Watch for changes
	 * in ViewJFrameBase that might break this method.
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
	            currentImage = imageA;
	            VOIs = imageA.getVOIs();
	        } else if (displayMode == IMAGE_B) {
	            currentImage = imageB;
	            VOIs = imageB.getVOIs();
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
	        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { 
	        	// voiFileDir.delete();
	        } else { // voiFileDir does not exist
	            voiFileDir.mkdir();
	        }
	
	        nVOI = VOIs.size();
	
	        System.err.println("Number of VOIs: " + nVOI);
	
	        for (i = 0; i < nVOI; i++) {
	            fileVOI = new PlugInFileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, currentImage);
	            fileVOI.writeVOI(VOIs.VOIAt(i), true);
	        }
	
	    } catch (IOException error) {
	        MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
	    }
	
	} // end saveAllVOIsTo()

	/**Loads relevant VOIs for the particular slice. */
	public void setSlice(int slice) {
		
		componentImage.setSlice(slice);
	    controls.setZSlider(componentImage.getSlice());
	    updateImages(true);
	    
	    // livewire grad mag. should be recalculated for the new slice    
	    if(currentSlice != slice) {
	    	for(int i=0; i<tabs.length; i++) 
				if(tabs[i].isVisible())
					tabs[i].setSlice(slice);
	    	currentSlice = slice;
	    }
	    
	    componentImage.getVOIHandler().resetLivewire();
	    componentImage.getVOIHandler().setCenter( new Vector3f( 0, 0, componentImage.getSlice() ) );
	    setTitle();
	}

	/**
	 * Unlocks the dialog, allowing movement between tabs while closing the current process.
	 * 
	 * @param closingTabLoc the tab to remove
	 */
	public void unlockToPanel(int closingTabLoc) {
		tabs[closingTabLoc].setVisible(false);
		dialogTabs.remove(tabs[closingTabLoc]);
	    for(int i=0; i<voiTabLoc; i++) 
	        dialogTabs.setEnabledAt(i, true);
	    dialogTabs.setSelectedIndex(activeTab);
	    voiChangeState = false;
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
	 * Captures the entire currently displayed image, not just area being shown in frame.
	 */
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
	 * Creates and initializes the component image for the given image.
	 *
	 * @param   extents  the image dimensionality.
	 *
	 * @throws  OutOfMemoryError  if enough memory cannot be allocated for this method
	 */
	protected void initComponentImage(int[] extents) throws OutOfMemoryError {
	
	    componentImage = new PlugInMuscleEditImage(this, imageA, LUTa, imageBufferA, null, null, imageBufferB,
	                                                 pixBuffer, zoom, extents, logMagDisplay,
	                                                 FileInfoBase.UNKNOWN_ORIENT);
	
	    componentImage.setBuffers(imageBufferA, imageBufferB, pixBuffer, pixBufferB);
	
	    if (resols[1] >= resols[0]) {
	        componentImage.setResolutions(1, heightResFactor);
	    } else {
	        componentImage.setResolutions(widthResFactor, 1);
	    }
	
	    // if this is a color image, then update the RGB info in the component
	    if (imageA.isColorImage()) {
	
	        if (getRGBTA() == null) {
	            setRGBTA(initRGB(imageA));
	        }
	    } // end if image is an RGB type
	
	} // end initComponentImage()

	/**
	 * Adds the table of voi information to the pdf, adds the images (edge and QA), and closes the document
	 * @param edgeImage
	 * @param qaImage
	 */
	protected void PDFclose(java.awt.Image edgeImage, java.awt.Image qaImage) {
			try {
			Paragraph aPar = new Paragraph();
			aPar.setAlignment(Element.ALIGN_CENTER);
			aPar.add(new Paragraph());
			if(multipleSlices) {
				aPar.add(new Chunk("Volume calculations", new Font(Font.TIMES_ROMAN, 14, Font.BOLD)));
			} else {
				aPar.add(new Chunk("Area calculations", new Font(Font.TIMES_ROMAN, 14, Font.BOLD)));
			}
			aPar.add(wholeTable);
			pdfDocument.add(new Paragraph());
			pdfDocument.add(aPar);
			if(multipleSlices) {
				for(int i=0; i<getActiveImage().getExtents()[2]; i++) {
					Paragraph slicePar = new Paragraph();
					slicePar.setAlignment(Element.ALIGN_CENTER);
					slicePar.add(new Paragraph());
					slicePar.add(new Chunk("Slice "+i, new Font(Font.TIMES_ROMAN, 14, Font.BOLD)));			
					slicePar.add(sliceTable[i]);
					pdfDocument.add(new Paragraph());
					pdfDocument.add(slicePar);
				}
			}
			
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
			pdfWriter.close();
			
			if(((AnalysisDialogPrompt)tabs[resultTabLoc]).calcOutputSuccess()) {
				MipavUtil.displayInfo("PDF saved to: " + pdfFile+"\nText saved to: "+textFile);
				ViewUserInterface.getReference().getMessageFrame().append("PDF saved to: " + pdfFile + 
											"\nText saved to: "+textFile+"\n", ViewJFrameMessage.DATA);
			} else {
				MipavUtil.displayError("<html>There was an error writing the output files.  "+
						"<br>Please make sure the image location is still available:<br>"+textFile.getParent());
			}
			
			} catch (Exception e) {
				e.printStackTrace();
				MipavUtil.displayError("<html>There was an error writing the output files.  "+
						"<br>Please make sure the image location is still available:<br>"+textFile.getParent());
				
			}
			
		}

	/**
	 * Creates the PDF file and creates several tables for scanner information as well
	 * as the VOI statistic information
	 *
	 */
	protected void PDFcreate(String fileDir, String fileName) {		
		if(!(new File(fileDir).exists())) {
			fileDir = getActiveImage().getFileInfo(getViewableSlice()).getFileDirectory()+VOI_DIR;
		}
		if(fileName == null) {
			fileName = fileDir + File.separator + "PDF_Report.pdf";
			pdfFile = new File(fileDir + File.separator + fileName);
			if(pdfFile.exists()) {
				int i=0;
				while(pdfFile.exists() && i<1000) {
					fileName = "PDF_Report-"+(++i)+ ".pdf";
					if(i == 1000) {
						MipavUtil.displayError("Too many PDFs have been created, overwriting "+fileName);
					}
					pdfFile = new File(fileDir + File.separator + fileName);
				}
			}
		} else {
			pdfFile = new File(fileDir + File.separator + fileName);
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
	
			FileInfoDicom fileInfo = (FileInfoDicom)getActiveImage().getFileInfo()[0];
			
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
			Date date = new Date();
			String dateStr = dateFormat.format(date);
			String userName = System.getProperty("user.name");
			
			// Comment here to return paragraph /**
			MultiColumnText mct = new MultiColumnText(20);
			mct.setAlignment(Element.ALIGN_LEFT);
			mct.addRegularColumns(pdfDocument.left(), pdfDocument.right(), 10f, 4);
			mct.addElement(new Paragraph("Analyst:", fontBold));
			mct.addElement(new Paragraph(userName, fontNormal));
			mct.addElement(new Paragraph("Analysis Date:", fontBold));
			mct.addElement(new Paragraph(dateStr, fontNormal));
			pdfDocument.add(mct);
			
			MultiColumnText mct2 = new MultiColumnText(20);
			mct2.setAlignment(Element.ALIGN_LEFT);
			mct2.addRegularColumns(pdfDocument.left(), pdfDocument.right(), 10f, 4);
			mct2.addElement(new Paragraph("Name:", fontBold));
			mct2.addElement(new Paragraph(getActiveImage().getFileInfo()[getViewableSlice()].getFileName(), fontNormal));
			mct2.addElement(new Paragraph("Center:", fontBold));
			String center = (String)fileInfo.getTagTable().getValue("0008,0080");
			mct2.addElement(new Paragraph((center != null ? center.trim() : "Unknown"), fontNormal));
			pdfDocument.add(mct2);
			
			MultiColumnText mct3 = new MultiColumnText(20);
			mct3.setAlignment(Element.ALIGN_LEFT);
			mct3.addRegularColumns(pdfDocument.left(), pdfDocument.right(), 10f, 4);
			mct3.addElement(new Paragraph("Patient ID:", fontBold));
			String id = (String)fileInfo.getTagTable().getValue("0010,0020");
			mct3.addElement(new Paragraph(((id != null && id.length() > 0) ? id.trim() : "Removed"), fontNormal));
			mct3.addElement(new Paragraph("Scan Date:", fontBold));
			String scanDate = (String)fileInfo.getTagTable().getValue("0008,0020");
			mct3.addElement(new Paragraph((scanDate != null ? scanDate.trim() : "Unknown"), fontNormal));
			pdfDocument.add(mct3);
			
			//add the scanning parameters table
			PdfPTable spTable = new PdfPTable(2);
			PdfPCell cell = new PdfPCell(new Paragraph("Scanning Parameters"));
			cell.setHorizontalAlignment(Element.ALIGN_CENTER);
			cell.setColspan(2);
			spTable.addCell(cell);
			spTable.addCell("kVp:");
			String kvp = (String)fileInfo.getTagTable().getValue("0018,0060");
			spTable.addCell((kvp != null ? kvp.trim() : "Unknown"));
			spTable.addCell("mA:");
			String mA = (String)fileInfo.getTagTable().getValue("0018,1151");
			spTable.addCell((mA != null ? mA.trim() : "Unknown"));
			spTable.addCell("Pixel Size: ("+FileInfoBase.getUnitsOfMeasureAbbrevStr(getActiveImage().getUnitsOfMeasure(0))+")");
			spTable.addCell(Double.toString(getActiveImage().getResolutions(0)[0]));
			spTable.addCell("Slice Thickness: ("+FileInfoBase.getUnitsOfMeasureAbbrevStr(getActiveImage().getUnitsOfMeasure(2))+")");
			spTable.addCell(Float.toString(getActiveImage().getResolutions(0)[2]));
			spTable.addCell("Table Height: (cm)");
			String height = (String)fileInfo.getTagTable().getValue("0018,1130");
			spTable.addCell((height != null ? height.trim() : "Unknown"));
			
			Paragraph pTable = new Paragraph();
			pTable.add(new Paragraph());
			pTable.setAlignment(Element.ALIGN_CENTER);
			pTable.add(spTable);
			pdfDocument.add(new Paragraph(new Chunk("")));
			pdfDocument.add(pTable);
			
			//create the Table where we will insert the data:
			wholeTable = new PdfPTable(new float[] {1.8f, 1f, 1f, 1f, 1f, 1f, 1f});
			// add Column Titles (in bold)
			String type = new String();
			String unit = FileInfoBase.getUnitsOfMeasureAbbrevStr(
										FileInfoBase.getUnitsOfMeasureFromStr(
												((AnalysisDialogPrompt)tabs[resultTabLoc]).getSelectedOutput()));
			if(multipleSlices) {
				wholeTable.addCell(new PdfPCell(new Paragraph("Volume ("+unit+"^3)", fontBold)));
				type = "Vol";	
			} else {
				wholeTable.addCell(new PdfPCell(new Paragraph("Area ("+unit+"^2)", fontBold)));
				type = "Area";
			}
			wholeTable.addCell(new PdfPCell(new Paragraph("Total "+type, fontBold)));
			wholeTable.addCell(new PdfPCell(new Paragraph("Fat "+type, fontBold)));
			wholeTable.addCell(new PdfPCell(new Paragraph("Lean "+type, fontBold)));
			wholeTable.addCell(new PdfPCell(new Paragraph("Fat HU", fontBold)));
			wholeTable.addCell(new PdfPCell(new Paragraph("Lean HU", fontBold)));
			wholeTable.addCell(new PdfPCell(new Paragraph("Total HU", fontBold)));
			
			if(multipleSlices) {
				sliceTable = new PdfPTable[getActiveImage().getExtents()[2]];
				for(int i=0; i<getActiveImage().getExtents()[2]; i++) {
					sliceTable[i] = new PdfPTable(new float[] {1.8f, 1f, 1f, 1f, 1f, 1f, 1f});
					sliceTable[i].addCell(new PdfPCell(new Paragraph("Area ("+unit+"^2)", fontBold)));
					type = "Area";
					sliceTable[i].addCell(new PdfPCell(new Paragraph("Total "+type, fontBold)));
					sliceTable[i].addCell(new PdfPCell(new Paragraph("Fat "+type, fontBold)));
					sliceTable[i].addCell(new PdfPCell(new Paragraph("Lean "+type, fontBold)));
					sliceTable[i].addCell(new PdfPCell(new Paragraph("Fat HU", fontBold)));
					sliceTable[i].addCell(new PdfPCell(new Paragraph("Lean HU", fontBold)));
					sliceTable[i].addCell(new PdfPCell(new Paragraph("Total HU", fontBold)));
				}
			}

			return;
		} catch (Exception e) {
		    ViewUserInterface.getReference().getMessageFrame().append("Error occured in addCell's calling method\n", ViewJFrameMessage.DEBUG);
		    e.printStackTrace();
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
	protected void PDFadd(String name, double fatArea, double leanArea, double totalAreaCount, 
			double meanFatH, double meanLeanH, double meanTotalH, PdfPTable aTable) {
		
	    try {
			Font fontNormal = FontFactory.getFont("Helvetica", 10, Font.NORMAL, Color.DARK_GRAY);
			if (name.endsWith(".xml")) {
				name = name.substring(0, name.length() - 4);
			}
			if(name == null) {
			    name = new String("Removed");
			    ViewUserInterface.getReference().getMessageFrame().append("Unexpected null encountered\n", ViewJFrameMessage.DEBUG);
			}
			ViewUserInterface.getReference().getMessageFrame().append("To test: "+name+"\n", ViewJFrameMessage.DEBUG);
			DecimalFormat dec = new DecimalFormat("0.00");
			
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
		    ViewUserInterface.getReference().getMessageFrame().append("Error adding PDF element.\n", ViewJFrameMessage.DEBUG);
		    if(wholeTable == null) 
			ViewUserInterface.getReference().getMessageFrame().append("aTable\n", ViewJFrameMessage.DEBUG);
		    if(name == null) 
			ViewUserInterface.getReference().getMessageFrame().append("name\n", ViewJFrameMessage.DEBUG);
		    e.printStackTrace();
		}
	}

	/**
	 * Performs automatic segmentation for various images.
	 */
	private void autoSegmentation() {
		if(imageType.equals(ImageType.Thigh)) {
			ModelImage srcImage = (ModelImage)getActiveImage().clone();
			if(voiBuffer.get("Left Bone").isEmpty() && voiBuffer.get("Right Bone").isEmpty()) {
		        ModelImage resultImage = (ModelImage)srcImage.clone();
		    	boneSeg = new PlugInAlgorithmCTBone(resultImage, srcImage, imageDir, voiBuffer.get("Left Bone").getColor());
		    	performSegmentation(boneSeg, resultImage);
			}
	    	
			if(voiBuffer.get("Left Marrow").isEmpty() && voiBuffer.get("Right Marrow").isEmpty()) {
		    	ModelImage resultImage2 = (ModelImage)srcImage.clone();
		    	marrowSeg = new PlugInAlgorithmCTMarrow(resultImage2, srcImage, imageDir, voiBuffer.get("Left Marrow").getColor());
		    	performSegmentation(marrowSeg, resultImage2);
			}
	    	
			if(voiBuffer.get("Left Thigh").isEmpty() && voiBuffer.get("Right Thigh").isEmpty()) {
		    	ModelImage resultImage3 = (ModelImage)srcImage.clone();
		    	thighSeg = new PlugInAlgorithmCTThigh(resultImage3, srcImage, imageDir, voiBuffer.get("Left Thigh").getColor());
		    	performSegmentation(thighSeg, resultImage3);
			}
		} else if(imageType.equals(ImageType.Abdomen)) {
			ModelImage srcImage = (ModelImage)getActiveImage().clone();
			if(voiBuffer.get("Abdomen").isEmpty()) {
				String[] options = {"Yes", "No"};
				String message = new String("The abdomen has not been segmented.  Would you like MIPAV to\n automatically segment"+
												" the abdomen and subcutaneous area?");		
				int segment = JOptionPane.showOptionDialog(this, message, "Automatic segmentation", JOptionPane.YES_NO_OPTION, 
															JOptionPane.QUESTION_MESSAGE, null, options, options[0]);
				if(segment == 0) {
	    			ModelImage resultImage = (ModelImage)srcImage.clone();
	    			abdomenSeg = new PlugInAlgorithmCTAbdomen(resultImage, srcImage, imageDir, voiBuffer.get("Abdomen").getColor());
	    			performSegmentation(abdomenSeg, resultImage);
				}
			}
		}
	}

	private JPanel buildMainPanel() {
		//The component image will be displayed in a scrollpane.       
	    scrollPane = new JScrollPane(componentImage, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
	                                 ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	    
	    componentImage.useHighlight(false);
	    scrollPane.setFocusable(true);
	    scrollPane.setBackground(Color.black);
	    //scrollPane.addKeyListener(this);
	    
	    dialogTabs = new JTabbedPane();
	    dialogTabs.setMinimumSize(new Dimension (370, 532));
	    dialogTabs.setPreferredSize(new Dimension(370, 532));
	    dialogTabs.setMaximumSize(new Dimension (370, 532));
	    
	    tabs = new DialogPrompt[mirrorArr.length+2]; //+2 for VOI and AnalysisPrompt
	    
	    JPanel panelA = new JPanel(new GridLayout(1, 3, 10, 10));
	    
	    JPanel testPanel = new JPanel();
	    testPanel.setLayout(new BorderLayout());
	    testPanel.add(dialogTabs, BorderLayout.WEST);
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
	    	dialogTabs.addTab((i+1)+": "+titles[i], tabs[i]);
	    }
	    //now put voiTab up
	    voiTabLoc = mirrorArr.length;
	    tabs[voiTabLoc] = new VoiDialogPrompt(this);
	    tabs[voiTabLoc].addListener(this);
	    tabs[voiTabLoc].addComponentListener(this);
	    tabs[voiTabLoc].setVisible(false);
	    
	    //now put resultsTab up
	    resultTabLoc = mirrorArr.length+1;
	    tabs[resultTabLoc] = new AnalysisDialogPrompt(this, mirrorArr, noMirrorArr);
	    tabs[resultTabLoc].addListener(this);
	    tabs[resultTabLoc].addComponentListener(this);
	    tabs[resultTabLoc].setVisible(false);
	            
	    return panelA;
	}

	/**
	 * Initializes the voiBuffer at program creation by loading all VOIs from the
	 * file system for the first time.
	 */
	private void createVOIBuffer() {
		Iterator<String> voiItr = voiBuffer.keySet().iterator();
		
		while(voiItr.hasNext()) {
			
			String name = voiItr.next();
			PlugInSelectableVOI v = loadVOI(name);
			ViewUserInterface.getReference().getMessageFrame().append("Just loaded: "+v.getName()+"\t has area: "+v.isEmpty()+"\n", ViewJFrameMessage.DEBUG);
			Color c = PlugInSelectableVOI.INVALID_COLOR;
			v.isEmpty();
			if((c = v.getColor()).equals(PlugInSelectableVOI.INVALID_COLOR)) {
	        	if((c = hasColor(v)).equals(PlugInSelectableVOI.INVALID_COLOR)) {
	                v.setColor(c = colorPick[colorChoice++ % colorPick.length]);
	        	} else {
	        		v.setColor(c);
	        	}
			} else {
				v.setColor(c);
			}
			voiBuffer.put(name, v);
			ViewUserInterface.getReference().getMessageFrame().append(voiBuffer.get(v.getName()) +"\twith area: "+ voiBuffer.get(v.getName()).isEmpty()+"\n", ViewJFrameMessage.DEBUG);
		}
	}

	/**
	 * Sets mode to CT and sets range to CT presets. Used for any new image.
	 *
	 * @param  preset1  first CT preset
	 * @param  preset2  second CT preset
	 */
	private void ctMode(ModelImage image, int preset1, int preset2) {
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

	/**
	 * Loads all VOIs for a particular pane by retrieving them from the VOI buffer.
	 * @param pane the current pane
	 */
	private void getVOIs(int pane) {
		
		Iterator<String> voiItr = voiBuffer.keySet().iterator();
		PlugInSelectableVOI v;
		while(voiItr.hasNext()) {
			String name = voiItr.next();
		
	    	if(voiBuffer.get(name).getLocation() == pane) {
	    		v = voiBuffer.get(name);
	    		v.setDisplayMode(VOI.BOUNDARY);
				getActiveImage().registerVOI(v);
	    	}
		}
	}

	/**
	 * Determines whether the mirror of the given VOI has a color; if so, returns it.
	 * Otherwise this method returns PlugInSelectableVOI.INVALID_COLOR.
	 */
	private Color hasColor(VOI voi) {
	    Color c = PlugInSelectableVOI.INVALID_COLOR;
	    Iterator<String> voiListItr = voiBuffer.keySet().iterator();
	    boolean colorFound = false;
	    String side1 = "", side2 = ""; 
	    if(Symmetry.LEFT_RIGHT == Symmetry.LEFT_RIGHT) {
	    	side1 = "Left";
	    	side2 = "Right";
	    } else if (Symmetry.TOP_BOTTOM == Symmetry.TOP_BOTTOM) {
	    	side1 = "Top";
	    	side2 = "Bottom";
	    }
	    String testName = new String();
	    while(voiListItr.hasNext() && !colorFound) {
	    	testName = voiListItr.next();
	        if(voi.getName().contains(side1) || voi.getName().contains(side2)) {
	            if( !(testName.contains(side1)  &&  voi.getName().contains(side1)) && 
	                    !(testName.contains(side2)  &&  voi.getName().contains(side2)) && 
	                    testName.endsWith(voi.getName().substring(voi.getName().indexOf(" ")))) {
	                c =  voiBuffer.get(testName).getColor();
	                colorFound = true;
	            }
	        }
	    }
	    return c;
	}

	private void initControls() {
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
	}

	/**
     * Initializes the VOI buttons for a particular pane.
     */
    private void initMuscleButtons(int pane) {
    	VOIVector vec = getActiveImage().getVOIs();
    	PlugInSelectableVOI temp = null;
    	for(int i=0; i<vec.size(); i++) 	
        	if((temp = voiBuffer.get(vec.get(i).getName())) != null && temp.getSliceSize(getViewableSlice()) > 0) 
        		((MuscleDialogPrompt)tabs[pane]).setButton(temp);
    }
    
    /**
     * Initializes the image for a given pane, loads the relevant VOIs and updates the correct buttons.
     * @param pane the current pane
     */
    private void initMuscleImage(int pane) {        
    	componentImage.setCursorMode(ViewJComponentBase.NEW_VOI);
    	getVOIs(pane);
        
        ViewUserInterface.getReference().getMessageFrame().append("Active tab: :"+activeTab+"\n", ViewJFrameMessage.DEBUG);
    	initMuscleButtons(pane);
        
    	this.repaint();

        updateImages(true);
    }
    
    private void initNext() {
	    
		long time = System.currentTimeMillis();
		JPanel mainPanel = buildMainPanel();
		
		//if this is standalone (app frame hidden), add the tabbedpane from the messageframe to the bottom of the plugin's frame
		if (ViewUserInterface.getReference().isAppFrameVisible()) {
			ViewUserInterface.getReference().getMessageFrame().append("Working on GUI", ViewJFrameMessage.DATA);
			mainPanel.setBackground(ViewUserInterface.getReference().getMainFrame().getBackground());
			getContentPane().setBackground(ViewUserInterface.getReference().getMainFrame().getBackground());
			setBackground(ViewUserInterface.getReference().getMainFrame().getBackground());
			getContentPane().add(mainPanel, BorderLayout.NORTH);
			getContentPane().update(getContentPane().getGraphics());
			getContentPane().validate();
			validate();
			pack();
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
	    ctMode(getImageA(), -175, 275);
	    initMuscleImage(2);
	    getActiveImage().unregisterAllVOIs();
	    initMuscleImage(1);
	    getActiveImage().unregisterAllVOIs();
	    initMuscleImage(0);
	    if (ViewUserInterface.getReference().isAppFrameVisible()) {
	    	this.setMinimumSize(new Dimension(380, 550));
	    } else {
	    	this.setMinimumSize(new Dimension(380, 640));
	    }
	    this.setResizable(true);
	    ViewUserInterface.getReference().getMessageFrame().append("Done2: "+(System.currentTimeMillis()-time)+"\n", ViewJFrameMessage.DEBUG);
	}

	/**
	 * Initializes the frame and variables.
	 */
	private void initStandAlone() throws OutOfMemoryError {
	    initResolutions();
	    initZoom();
	    initLUT();
	
	    int[] extents = createBuffers();
	
	    initComponentImage(extents);
	    initExtentsVariables(imageA);
	
	    initControls();
	            
	    // MUST register frame to image models
	    imageA.addImageDisplayListener(this);
	
	    windowLevel = new JDialogWinLevel[2];      
	    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
	
	    pack();
	    
	    // User interface will have list of frames
	    userInterface.registerFrame(this);
	
	    getContentPane().add(controls, BorderLayout.NORTH);
	    
	    //call the normal init function here, 
	    initNext();
	} // end init()

	/**
	 * Initializes the image of the a voiDialogPrompt by setting relevant VOIs to
	 * solid mode, note does not retrieve VOIs from the buffer 
	 */
	private void initVoiImage() {
		//VOIs of pane already loaded, just need to make relevant ones solid
	    VOIVector voiVec = getActiveImage().getVOIs();
	    VOI voi;
		for(int i=0; i<voiVec.size(); i++) { 
			if(voiBuffer.get((voi = voiVec.get(i)).getName()).getFillEligible()) {
				if(!(((VoiDialogPrompt)tabs[voiTabLoc]).getObjectName().equals(voi.getName()))) { 
					voi.setDisplayMode(VOI.SOLID);
				}
			}
		}
	    
	    componentImage.setCursorMode(ViewJComponentBase.NEW_VOI);
	    updateImages(true);
	}

	/**
	 * Performs a given segmentation on resultImage represented by the given algorithm
	 */
	private boolean performSegmentation(AlgorithmBase genericAlgo, ModelImage resultImage) {
		try {
	        String name = JDialogBase.makeImageName(getActiveImage().getImageName(), "_kidneys");
	        resultImage.setImageName(name);
	
	        // This is very important. Adding this object as a listener allows the algorithm to
	        // notify this object when it has completed or failed. See algorithm performed event.
	        // This is made possible by implementing AlgorithmedPerformed interface
	        genericAlgo.addListener(this);
	
	        if (genericAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	            System.err.println("A thread is already running on this object");
	            return false;
	        }
	            
	        return true;
	    } catch (OutOfMemoryError x) {
	        if (resultImage != null) {
	            resultImage.disposeLocal(); // Clean up memory of result image
	            resultImage = null;
	        }
	
	        System.err.println("Segmentation error: unable to allocate enough memory");
	
	        return false;
	    }
	}
	
	/**
	 * Propagate inverse of children relationship as dependents to keep calculations correct.
	 */
	private void setDependents() {
		Iterator itr = voiBuffer.values().iterator();
		
		while(itr.hasNext()) {
			PlugInSelectableVOI voi = (PlugInSelectableVOI)itr.next();
			for(int i=0; i<voi.getChildren().length; i++) {
				voi.getChildren()[i].addDependent(voi);
			}
		}
	}

	/**
	 * Waits for automatic segmentation algorithms to complete.
	 */
	private void waitAlg(ViewJProgressBar progressBar) {
		long time = System.currentTimeMillis();
		int extend = 0;
		if(imageType.equals(ImageType.Thigh)) {
	    	if(marrowSeg != null) {
	    		while(marrowSeg.isAlive()) {
	        		if((System.currentTimeMillis() - time) / 2000 > extend) {
	        			progressBar.updateValue(20+(++extend));
	        		}
	        	}
	        	progressBar.updateValue(60);
	        	progressBar.setMessage("Marrow segmentation complete...");
	        	ViewUserInterface.getReference().getMessageFrame().append("Marrow seg alg finished\n", ViewJFrameMessage.DEBUG);
	        }
	    	if(boneSeg != null) {
	    		extend = (int)(System.currentTimeMillis() - time) / 2000 - 1;
	        	while(boneSeg.isAlive()) {
	        		if((System.currentTimeMillis() - time) / 2000 > extend) {
	        			progressBar.updateValue(60+(++extend));
	        		}
	        	}
	        	progressBar.updateValue(70);
	        	progressBar.setMessage("Bone segmentation complete...");
	        	ViewUserInterface.getReference().getMessageFrame().append("Bone seg alg finished\n", ViewJFrameMessage.DEBUG);
	        }
	        if(thighSeg != null) {
	        	extend = (int)(System.currentTimeMillis() - time) / 2000 - 1;
	        	while(thighSeg.isAlive()) {
	        		if((System.currentTimeMillis() - time) / 2000 > extend) {
	        			progressBar.updateValue(70+(++extend));
	        		}
	        	}
	        	progressBar.setMessage("Thigh segmentation complete...");
	        	ViewUserInterface.getReference().getMessageFrame().append("Thigh seg alg finished\n", ViewJFrameMessage.DEBUG);
	        }
		} else if(imageType.equals(ImageType.Abdomen)) {
			if(abdomenSeg != null) {
	    		while(abdomenSeg.isAlive()) {
	        		if((System.currentTimeMillis() - time) / 5000 > extend) {
	        			progressBar.updateValue(20+(++extend));
	        		}
	        	}
	        	progressBar.updateValue(70);
	        	progressBar.setMessage("Abdomen segmentation complete...");
	        	ViewUserInterface.getReference().getMessageFrame().append("Abdomen seg alg finished\n", ViewJFrameMessage.DEBUG);
	        }
		}
	}

	/**
     * Abstract class to represent any of the dialog prompts.  Classes that extend this one
     * need to initialize their own dialog as well as create an actionedPerformed() method 
     * to handle button calls.
     * 
     * @author senseneyj
     *
     */
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
    	public static final String SELECT_ALL = "Select all";
    	public static final String TOGGLE_LUT = "Toggle LUT";
    	public static final String OUTPUT = "Output";
    	public static final String BACK = "Back";
    	public static final String RESET = "Reset";
    	public static final String HIDE_ONE = "Hide except";
    	
    	/** The group of buttons in this dialog prompt*/
    	protected JButton buttonGroup[];
    	
    	/**
		 * default button list
		 */
		protected String buttonStringList[] = { OK, HIDE_ALL, HELP };
		protected boolean completed = false;
		/**
		 * The containing frame
		 */
		protected PlugInMuscleImageDisplay muscleFrame;
		/**
		 * The title of this DialogPrompt
		 */
		protected String title;
		/**
		 * The ActionListeners of a given initialization of this class
		 */
		private Vector<ActionListener> objectList = new Vector<ActionListener>();
		/**
    	 * Constructor requires containing frame and title, initializes using default button
    	 * list unless another is specified through setButtons.
    	 * @param theParentFrame the containing frame
    	 * @param title
    	 */
    	public DialogPrompt(PlugInMuscleImageDisplay theParentFrame, String title) {
    		this.muscleFrame = theParentFrame;
    		this.title = title;
    	}
    	
    	/**
    	 * Constructor requires containing frame, title and a list of buttons. 
    	 * @param theParentFrame the containg frame
    	 * @param title
    	 * @param buttonString the list of buttons to replace buttonStringList
    	 */
    	public DialogPrompt(PlugInMuscleImageDisplay theParentFrame, String title, String[] buttonString) {
    		this.muscleFrame = theParentFrame;
    		this.title = title;
    		this.buttonStringList = buttonString;
    	}
    	
    	/**
		 * Abstract actionPerformed required to deal with events such as button clicks.
		 */
		public abstract void actionPerformed(ActionEvent e);

		/**
		 * Abstract method for dealing with a change in slice.
		 * @param slice
		 */
		public abstract void setSlice(int slice);

		/**
		 * Abstract method required for initializing display.
		 */
		protected abstract void initDialog();

		/**
		 * Add a listener to this class so that when when the dialog has completed processing it can use notifyListener
		 * to notify all listeners that the dialog has completed.
		 *
		 * @param  obj  AlgorithmInterface "object' to be added to the list
		 */
		public void addListener(ActionListener obj) {
			objectList.addElement(obj);
		}

		public boolean getCompleted() {
			return completed;
		}

		/**
    	 * Returns the current title of this dialog prompt.
    	 */
    	public String getTitle() {
    		return title;
    	}
    	
    	/**
		 * Builds button panel consisting of buttonStringList.  If none is specified uses
		 * default OK, CANCEL, and HELP
		 *
		 * @return  JPanel that has OK, CANCEL, and HELP buttons
		 */
		protected JPanel buildButtons() {
		    JPanel buttonPanel = new JPanel();
		    buttonGroup = new JButton[buttonStringList.length];
		    
		    if (buttonGroup.length > 3) {
		    	JPanel topPanel = new JPanel();
		    	JPanel centerPanel = new JPanel();
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
		    			buttonGroup[i].setMaximumSize(new Dimension(300, 300));
		    		}
		    		buttonGroup[i].setFont(MipavUtil.font12B);
		    	
		    		if (i < 3) {
		    			topPanel.add(buttonGroup[i]);
		    		} else if(i < 6) {
		    			centerPanel.add(buttonGroup[i]);
		    		} else {
		    			bottomPanel.add(buttonGroup[i]);
		    		}
		    		
		    	}
		    	buttonPanel.setLayout(new BorderLayout());
		    	buttonPanel.add(topPanel, BorderLayout.NORTH);
		    	buttonPanel.add(centerPanel, BorderLayout.CENTER);
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
    	 * Replaces buttonStringList with the given buttons.  
    	 * @param buttonString
    	 */
    	protected void setButtons(String[] buttonString) {
    		this.buttonStringList = buttonString;
    	}
    }

    private class VoiDialogPrompt extends DialogPrompt implements ActionListener, AlgorithmInterface {

        /**Valid buttons for this DialogPrompt. */
    	protected final String[] buttonStringList = {OK, HIDE_ALL, CANCEL, RESET, HIDE_ONE};
    	
    	/**The name of the current VOI*/
        private String objectName;
        
        /**Whether the current VOI is closed, also accesible via voiBuffer.get(objectName).isClosed()*/
        private boolean closedVoi;
        
        /**The number of curves that can be created, also reached via voiBuffer.get(objectName).getNumOfMaxCurves()*/
        private int numVoi;
        
        /**Whether the voiExists when dialogPrompt is first called*/
        private boolean voiExists;
        
        /**Describes the voi properties (ex. 1 closed curve)*/
        private JLabel selectText;

        /**Warns user that VOI was created by MIPAV*/
        private JLabel warningText;
        
        /**Buffer for hide/show functions related to HIDE_ALL button**/
        private VOIVector voiPromptBuffer;
        
        /**Snake algorithm for 3D propagating operation/ */
        private AlgorithmSnake snakeAlgo;
        
        /**Progress bar for snake algorithm*/
        private ViewJProgressBar snakeProgress;

        /**Propagation menu for smoothing if 3D image*/
        private JMenu propMenu;
        
        /**VOI under investigation, to be replaced if cancel button is pressed.*/
        private PlugInSelectableVOI voiInvestigated = null;
        
        /**
         * Constructor that only requires containing frame. Title equals "VOI"
         * @param theParentFrame the containing frame.
         */
        public VoiDialogPrompt(PlugInMuscleImageDisplay theParentFrame) {
            super(theParentFrame, "VOI");

            setButtons(buttonStringList);
            
            voiPromptBuffer = new VOIVector();
            
            initDialog();
        }
        
        /**
         * Implementing abstract method to handle dialog buttons
         */
        public void actionPerformed(ActionEvent e) {

            String command = e.getActionCommand();

            if(command.equals(HIDE_ALL)) {
                //clear all VOIs drawn
            	VOIVector voiPromptBufferAppend = (VOIVector)getActiveImage().getVOIs().clone();
            	for(int i=0; i<voiPromptBufferAppend.size(); i++) {
            		voiPromptBuffer.add(voiPromptBufferAppend.get(i));
            	}
                getActiveImage().unregisterAllVOIs();
                ((JButton)e.getSource()).setText(SHOW_ALL);
                ((JButton)e.getSource()).setActionCommand(SHOW_ALL);
                updateImages(true);
            } else if(command.equals(SHOW_ALL)) {
            	//show all VOIs previously drawn, do not get rid of any VOIs on screen
            	VOIVector voiPromptBufferAppend = (VOIVector)getActiveImage().getVOIs().clone();
            	for(int i=0; i<voiPromptBufferAppend.size(); i++) {
            		voiPromptBuffer.add(voiPromptBufferAppend.get(i));
            	}
        		getActiveImage().unregisterAllVOIs();
            	for(int i=0; i<voiPromptBuffer.size(); i++) {
            		getActiveImage().registerVOI(voiPromptBuffer.get(i));
            	}
            	((JButton)e.getSource()).setText(HIDE_ALL);
                ((JButton)e.getSource()).setActionCommand(HIDE_ALL);
                voiPromptBuffer.removeAllElements();
                updateImages(true);
            } else if (command.equals(OK)) {
            
                    PlugInSelectableVOI goodVoi = checkVoi(); //check voi has correct number of curves, etc
                    if ( goodVoi != null ) { 
                        voiChangeState = true;
                        //save modified/created VOI to file
                        getActiveImage().unregisterAllVOIs();
                        getActiveImage().registerVOI(goodVoi);
                        String dir = getImageA().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+File.separator;
                        saveAllVOIsTo(dir);

                        MipavUtil.displayInfo(objectName+" VOI saved in folder\n " + dir);
                        completed = true;
                        warningText.setText("");
                        PlugInSelectableVOI voi;
                        (voi = voiBuffer.get(goodVoi.getName())).setCreated(true);
                        voi.setComputerGenerated(false);
                        for(int i=0; i<buttonGroup.length; i++) {
                    		if(buttonGroup[i].getText().equals(SHOW_ALL)) {
                    			buttonGroup[i].setText(HIDE_ALL);
                    			buttonGroup[i].setActionCommand(HIDE_ALL);
                    		} 
                    	}
                        voi.setLastModified(System.currentTimeMillis());
                        //always perform new calculation here, no need to check
                        if(voiBuffer.get(objectName).getCalcEligible()) {
	                        MuscleCalculation muscleCalc = new MuscleCalculation(goodVoi, objectName);
	                        Thread calc = new Thread(calcGroup, muscleCalc, objectName);
	                        calc.start();
	                        PlugInSelectableVOI[] dependents = voiBuffer.get(objectName).getDependents();
	                        //Perform calculations on all VOIs that depend on this VOI
	                        for(int i=0; i<dependents.length; i++) {
	                        	MuscleCalculation muscleCalcDep = new MuscleCalculation(dependents[i], dependents[i].getName());
	                        	Thread calcDep = new Thread(calcGroup, muscleCalcDep, dependents[i].getName());
		                        calcDep.start();
	                        }
                        }
                        
                        getActiveImage().unregisterAllVOIs();
                        updateImages(true);
                    } else {
                    	completed = false;	//error state, does not return to main dialog, waits for user
                    }
                } else if (command.equals(CANCEL)) {
                	voiChangeState = true;
                	for(int i=0; i<buttonGroup.length; i++) {
                		if(buttonGroup[i].getText().equals(SHOW_ALL)) {
                			buttonGroup[i].setText(HIDE_ALL);
                			buttonGroup[i].setActionCommand(HIDE_ALL);
                		} 
                	}
                	getActiveImage().unregisterAllVOIs();
                	if(voiInvestigated != null) 
                		voiBuffer.put(objectName, loadVOI(objectName));
                } else if (command.equals(RESET)) {
                	for(int i=0; i<buttonGroup.length; i++) {
                		if(buttonGroup[i].getText().equals(SHOW_ALL)) {
                			buttonGroup[i].setText(HIDE_ALL);
                			buttonGroup[i].setActionCommand(HIDE_ALL);
                		}
                	}
                	voiPromptBuffer.removeAllElements();
                	getActiveImage().unregisterAllVOIs();
                	initMuscleImage(activeTab);
                	initVoiImage(); //replacing current image and updating
                	updateImages(true);
                } else if (command.equals(HIDE_ONE)) {
                	//cannot hide one if hide all has been changed to show all
                	boolean hideEligible = false;
                	for(int i=0; i<buttonGroup.length; i++) {
                		if(buttonGroup[i].getText().equals(HIDE_ALL)) {
                			buttonGroup[i].setText(SHOW_ALL);
                			buttonGroup[i].setActionCommand(SHOW_ALL);
                			hideEligible = true;
                		} 
                	}
                	if(hideEligible) {
	                	voiPromptBuffer = (VOIVector)getActiveImage().getVOIs().clone();
	                	VOIVector vec = getActiveImage().getVOIs();
	                	VOI goodVoi = null;
	                	for(int i=0; i<vec.size(); i++) {
	                		if(vec.get(i).getName().equals(objectName)) {
	                			goodVoi = vec.get(i);
	                			voiPromptBuffer.remove(i);
	                		}
	                	} //note: does unregister/register to for faster method execution
	                	getActiveImage().unregisterAllVOIs();
	                    if(goodVoi != null) {
	                    	getActiveImage().registerVOI(goodVoi);
	                    }
	                	updateImages(true);
                	}
                	
                } else if (command.equals("PropVOIUp")) {
                	if (componentImage.getVOIHandler().propVOI(1, false) == true) {
                        incSlice();
                    }
                } else if (command.equals("PropVOIDown")) {
                	if (componentImage.getVOIHandler().propVOI(-1, false) == true) {
                        decSlice();
                    }
                } else if (command.equals("PropVOIAll")) {
                	componentImage.getVOIHandler().propVOIAll();
                } 
            }
            
        /**Method for dealing with algorithms*/
        public void algorithmPerformed(AlgorithmBase algorithm) {
			if(algorithm instanceof AlgorithmSnake) {
				snakeProgress.updateValue(75);
				ViewUserInterface.getReference().getMessageFrame().append("Performed successfully\n", ViewJFrameMessage.DEBUG);
				getActiveImage().registerVOI(snakeAlgo.getResultVOI());
				
				//getActiveImage().unregisterAllVOIs();
				//for(int i=0; i<voiPromptBuffer.size(); i++) {
				//	getActiveImage().registerVOI(voiPromptBuffer.get(i));
				//}
				snakeProgress.updateValue(95);
				snakeProgress.dispose();
			}
		}

		/**
		 * @return the current VOI
		 */
		public String getObjectName() {
		    return objectName;
		}

		public void setSlice(int slice) {
			//Do nothing since this dialog does not depend on a changing slice (though could in the future)
		}

		/**
		 * Sets up the dialog given the VOI name, uses voiBuffer.get(name).X() for initializing
		 * all other VOI properties.
		 * @param name the name of the VOI
		 */
		public void setUpDialog(String name) {	
			this.objectName = name;
			this.closedVoi = voiBuffer.get(objectName).isClosed();
			this.numVoi = voiBuffer.get(objectName).getMaxCurvesPerSlice();
			warningText.setText("");
			
			voiPromptBuffer.removeAllElements();
			
			voiExists = voiBuffer.get(objectName).getCreated();
			if(voiExists)
				voiInvestigated = ((PlugInSelectableVOI)voiBuffer.get(objectName).clone());
			
			for(int i=0; i<buttonGroup.length; i++) {
		    	if(buttonGroup[i].getText().contains(HIDE_ONE)) {
		    		buttonGroup[i].setText(HIDE_ONE+" "+name.toLowerCase());
		    		buttonGroup[i].setPreferredSize(new Dimension(185, 30));
		    	}
		    }
		    
		    updateSelectionLabel();
		    
		    if(voiExists && voiBuffer.get(name).isComputerGenerated()) {
		    	createWarningLabel();
		    }
		}

		/**
		 * Prepares to leave voi dialog, clears the voiPromptBuffer.
		 */
		public void takeDownDialog() {
			removeAll();
			voiPromptBuffer.removeAllElements();
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
		    
		    selectText = new JLabel("");
		    warningText = new JLabel("");
		    
		    selectText.setFont(MipavUtil.font12);
		    warningText.setFont(MipavUtil.font12);
		    mainPanel.add(selectText, BorderLayout.NORTH);
		    mainPanel.add(warningText, BorderLayout.CENTER);
		    
		    
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
		
		private PlugInSelectableVOI checkVoi() {
		    VOIVector srcVOI = muscleFrame.getActiveImage().getVOIs();
		    //equal to numVoi when the right  amount of VOIs have been created
		    int countQualifiedVOIs = 0;
		    VOI goodVOI = null;
		    //see if the VOI has been modified
		    boolean curveReplace = false;
		    for(int i=0; i<srcVOI.size(); i++) {
		        if(srcVOI.get(i).getName().equals(objectName)) {
		            if(srcVOI.get(i).getSliceSize(getViewableSlice()) > 0) {
		            	goodVOI = (VOI)srcVOI.get(i).clone();
		            	countQualifiedVOIs++;
		            	curveReplace = true;
		            } 
		        }
		    }
		    if(countQualifiedVOIs != 1) {
		        //else VOI no longer exists, look for a VOI that doesn't fit to call objectName
		        for(int i=0; i<srcVOI.size(); i++) {
		        	if(getLocationStatus(srcVOI.get(i).getName()) == PlugInSelectableVOI.INVALID_LOC_NUMBER) {
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
		    
		    int zDim = muscleFrame.getActiveImage().getExtents().length > 2 ? 
		            muscleFrame.getActiveImage().getExtents()[2] : 1;
		    Vector<VOIBase>[] curves = goodVOI.getSortedCurves(zDim);           
		
		    boolean maxCurve = false, closedProblem = false;
		    int sliceCurves = 0, sliceClosed = 0, slice=0;
		    while(slice<curves.length && !maxCurve) {
		    	if(curves[slice].size() > numVoi) { 
		    		maxCurve = true;
		    		sliceCurves = slice;
		    	}
		    	for(int j=0; j<curves[slice].size(); j++) {
		    		if(((VOIContour)curves[slice].get(j)).isClosed() != closedVoi) {
		    			closedProblem = true;
		    			sliceClosed = slice;
		    		}
		    	}
		    	slice++;
		    }
		    
		    if(maxCurve) {
		    	String error = "You have created too many curves";
		    	if(multipleSlices)
		    		error += " on slice "+sliceCurves;
		    	error += ". Please remove these curves.";
		        MipavUtil.displayError(error);
		        return null;
		    }
		    
		    if(closedProblem) {
		    	String error = closedVoi ? "Any curves made must be closed." : 
		            "Any curves made must be open.";
		    	if(multipleSlices)
		    		error += " Please correct the curve on slice "+sliceClosed+".";
		    	MipavUtil.displayError(error);
		    	return null;
		    }
		    

            if(curveReplace) 
                voiBuffer.get(objectName).getCurves().removeAllElements();
			for(int i=0; i<curves.length; i++) { 
				for(int j=0; j<curves[i].size(); j++) {
				    voiBuffer.get(objectName).importCurve((VOIContour)curves[i].get(j));
				}
			}
		    
		    return voiBuffer.get(objectName);
		}

		/**
         * Creates a warning label to notify the user that the VOI was created by MIPAV and has not been checked.
         */
        private void createWarningLabel() {
        	String warning = "<html>NOTE: This VOI was created by MIPAV and has not been checked."+
        						" Please review this VOI carefully.</html>";
        	
        	warningText.setText(warning);
        	warningText.setForeground(Color.RED);
        }
        
        /**
		 * Gets the VOI that should have the snake algorithm performed on it.
		 * @return the VOI to snake
		 */
		private VOI getInterestingVOI() {
			VOIVector testVec = getActiveImage().getVOIs();
			VOI goodVOI = null;
			for(int i=0; i<testVec.size(); i++) {
				if(testVec.get(i).isActive()) {
					return goodVOI = testVec.get(i);	
				}
			}
			for(int i=0; i<testVec.size(); i++) {
				if(testVec.get(i).equals(objectName)) {
					return goodVOI = testVec.get(i);
				}
			}
			return goodVOI;
		}

		/**
		 * Performs snaking operation on a pre-selected VOI of given tape propagation type
		 * @param propagationType
		 */
		private void performSnake(int propagationType) {
			VOI v = null;
			if((v = getInterestingVOI()) == null) {
				MipavUtil.displayError("Please select a VOI");
				return;
			}
			snakeProgress = new ViewJProgressBar("Propogate", "Propagating...", 0, 100, false);
			VOIVector tempBuffer = getActiveImage().getVOIs();
			tempBuffer.remove(v);
			voiPromptBuffer.removeAllElements();
			voiPromptBuffer.addAll(tempBuffer);
		    float[] sigmas = {(float)1.0, (float)1.0, getActiveImage().getExtents()[2]};
		    int boundaryIterations = 50;
		    int smoothness = 2;
		    int boundaryDir = AlgorithmSnake.IN_DIR; 
		    snakeAlgo = new AlgorithmSnake(getActiveImage(), sigmas, boundaryIterations, smoothness, v, boundaryDir);
		
		    snakeAlgo.setPropagation(propagationType);
		    
		    snakeAlgo.addListener(this);
		    snakeProgress.updateValue(25); 
		    if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
		       MipavUtil.displayError("A thread is already running on this object");
		    }
		    snakeProgress.updateValue(52);
		}

		/**
		 * Updates the selection label for each VOI.  The calling method will have updated 
		 * closedVoi, numVoi, voiExists, and objectName
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
    }
    
    /**
     * Instances of this class are main dialog prompts that allow users to view created VOIs
     * and to select VOIs to modify in a VoiDialogPrompt.
     * 
     * @author senseneyj
     *
     */
    private class MuscleDialogPrompt extends DialogPrompt implements ActionListener {
        
        //~ Static fields/initializers -------------------------------------------------------------------------------------
    
        public static final int REMOVED_INTENSITY = -2048;
        
        public static final String CHECK_BOX = "CHECK_BOX";
        
        private final String[] buttonStringList = {CALCULATE, HELP, EXIT};
        
        //~ Instance fields ------------------------------------------------------------------------------------------------
        
        /** Denotes the anatomical part represented in the image. Implemented in case this class
         *  is moved to its own class at a later time.  
         */
        private ImageType imageType;
        
        /** Whether this image has mirror image muscles (eg VIEWS of thighs, abdomen. */
        private Symmetry symmetry;
        
        /** Labels for instructions. */
        private JLabel[] instructionLabel;
        
        /** Check boxes for mirror object buttons. */
        private PlugInMuscleColorButtonPanel[] mirrorCheckArr;
        
        /** Check boxes for non-mirror object buttons. */
        private PlugInMuscleColorButtonPanel[] noMirrorCheckArr;

        /** Buttons for muscles where a mirror muscle may exist. */
        private JButton[] mirrorButtonArr;
        
        /** Buttons for muscles where mirror muscles are not considered. */
        private JButton[] noMirrorButtonArr;
        
        /** Text for muscles where a mirror muscle may exist. */
        private String[] mirrorArr;
        
        /** Text for muscles where mirror muscles are not considered. */
        private String[] noMirrorArr;

        /**Allows dialog to know its place in the world. */
        private int index;
        
        /**
         * Creates new set of prompts for particular muscle.
         *
         * @param  theParentFrame  Parent frame.
         */
        public MuscleDialogPrompt(PlugInMuscleImageDisplay theParentFrame, String title, String[] mirrorArr, 
                String[] noMirrorArr, ImageType imageType, Symmetry symmetry, int index) {

            super(theParentFrame, title);
            
            setButtons(buttonStringList);
            
            this.mirrorArr = mirrorArr;
            this.noMirrorArr = noMirrorArr;
            
            this.imageType = imageType;
            this.symmetry = symmetry;
            
            this.index = index;
            initDialog();    
        }
        
        //TODO: Put PlugInMuscleImageDisplay's actions here
		//calculate, help, exit
		public void actionPerformed(ActionEvent e) {
		    ViewUserInterface.getReference().getMessageFrame().append("Evaluating colorButtonPanel\n", ViewJFrameMessage.DEBUG);
		    if(e.getSource() instanceof PlugInMuscleColorButton) {
            	PlugInMuscleColorButton obj = ((PlugInMuscleColorButton)e.getSource());
		    	if (obj.getColorIcon().getColor() != Color.BLACK) {
            		VOIVector vec = getActiveImage().getVOIs();
            		for(int i=0; i<vec.size(); i++) {
            			vec.get(i).setAllActive(false);
	            		if(vec.get(i).getName().equals(obj.getVOIName())) {
	            			vec.get(i).setAllActive(true);
	            		}
            		}
            		updateImages(true);
            		
            		getComponentImage().getVOIHandler().showColorDialog();
            	}
		    }
		}

		/**The location of this dialog in the parentFrame's array*/
        public int getIndex() {
        	return index;
        }
        
        /**
		 * Gets the symmetric buttons in this panel.
		 */
		public JButton[] getMirrorButton() {
			if(mirrorButtonArr != null) {
				return mirrorButtonArr;
			}
			return new JButton[0];
		}

		/**
		 * Gets the non-symmetric buttons in this panel.
		 */
		public JButton[] getNoMirrorButton() {
			if(noMirrorButtonArr != null) {
				return noMirrorButtonArr;
			}
			return new JButton[0];
		}

		/**
		 * Sets a particular button to the correct colors.  Used in initialization.
		 */
		public void setButton(PlugInSelectableVOI v) {
			String name = v.getName();
			Color c = v.getColor();
			if(!c.equals(Color.BLACK)) 
				voiBuffer.get(name).setCreated(true);
			for(int i=0; i<mirrorButtonArr.length; i++) {
				if(name.equals(mirrorButtonArr[i].getText())) {
					mirrorCheckArr[i].setColor(c);
					v.addVOIListener(mirrorCheckArr[i].getColorButton());
					if(v.isComputerGenerated()) {
						mirrorButtonArr[i].setForeground(Color.RED);
					} else {
						mirrorButtonArr[i].setForeground(Color.BLACK);
					}
				}
			}
			for(int i=0; i<noMirrorButtonArr.length; i++) {
				if(name.equals(noMirrorButtonArr[i].getText())) {
					noMirrorCheckArr[i].setColor(c);
					v.addVOIListener(noMirrorCheckArr[i].getColorButton());
					if(v.isComputerGenerated()) {
						noMirrorButtonArr[i].setForeground(Color.RED);
					} else {
						noMirrorButtonArr[i].setForeground(Color.BLACK);
					}
				}
			}
		}

		/**
         * Implemented abstract method for dealing with slice changes, currently sets check boxes of
         * VOIs that have a curve on the given slice to that VOIs color.
         */
        public void setSlice(int slice) {
        	PlugInSelectableVOI temp;
        	for(int i=0; i<mirrorCheckArr.length; i++) {
        		if((temp = voiBuffer.get(mirrorButtonArr[i].getText())).getSliceSize(slice) == 0) {
	        		mirrorCheckArr[i].setColor(Color.black);
	        		mirrorCheckArr[i].getColorButton().colorChanged(Color.black);
        		} else {
        			mirrorCheckArr[i].setColor(temp.getColor());
	        		mirrorCheckArr[i].getColorButton().colorChanged(temp.getColor());
        		}
        		//ViewUserInterface.getReference().getMessageFrame().append("Size of "+temp.getName()+": "+temp.getCurves()[slice].size());
        	}
        	for(int i=0; i<noMirrorCheckArr.length; i++) {
        		if((temp = voiBuffer.get(noMirrorButtonArr[i].getText())).getSliceSize(slice) == 0) {
	        		noMirrorCheckArr[i].setColor(Color.black);
	        		noMirrorCheckArr[i].getColorButton().colorChanged(Color.black);
        		} else {
        			noMirrorCheckArr[i].setColor(temp.getColor());
	        		noMirrorCheckArr[i].getColorButton().colorChanged(temp.getColor());
        		}	
        		//ViewUserInterface.getReference().getMessageFrame().append("Size of "+temp.getName()+": "+temp.getCurves()[slice].size());
        	}
        }
        
        /**
         * Implemented abstract method constructs dialog.
         */
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
            
            mirrorCheckArr = new PlugInMuscleColorButtonPanel[mirrorArr.length * 2];
            mirrorButtonArr = new JButton[mirrorArr.length * 2];
            if(mirrorArr.length > 0) {
            	gbc.gridy++;
            	add(initSymmetricalObjects(), gbc);
            }
            
            noMirrorCheckArr = new PlugInMuscleColorButtonPanel[noMirrorArr.length];
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

		/**
		 * Builds the panel of instructions for this dialog.
		 */
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
		    instructionLabel[0] = new JLabel("1) Press an object button.");
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

		/**
		 * Builds the panel of non-symmetrical objects for the dialog box initialization.
		 */
		private JPanel initNonSymmetricalObjects() {
		
		    ButtonGroup noMirrorGroup = new ButtonGroup();
		    JPanel noMirrorPanel = new JPanel(new GridBagLayout());
		    noMirrorPanel.setForeground(Color.BLACK);
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
		      
		        noMirrorCheckArr[i] = new PlugInMuscleColorButtonPanel(Color.black, noMirrorArr[i], this);
		        
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

		/**
		 * Builds the panel of symmetrical objects for the dialog box initialization.
		 */
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
		
		        mirrorCheckArr[i] = new PlugInMuscleColorButtonPanel(Color.BLACK, mirrorButtonArr[i].getText(), this);        
		       
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
    }
    
    /**
     * This extension of DialogPrompt displays performed calculations.
     * 
     * @author senseneyj
     *
     */
    private class AnalysisDialogPrompt extends DialogPrompt implements ActionListener {
		
    	//~ Static fields/initializers -------------------------------------------------------------------------------------
    	
    	public static final String LOAD_VOI = "Load VOI";
    	
    	private final String[] buttonStringList = {OUTPUT, SELECT_ALL, SAVE, TOGGLE_LUT, HELP, BACK};
	
    	//~ Instance fields -------------------------------------------------------------------------------------
    	
    	/** Labels for instructions. */
		private JLabel instructionLabel;

		/** Text for muscles where a mirror muscle may exist. */
		private String[][] mirrorCalcItemsArr;

		/** Text for muscles where mirror muscles are not considered. */
		private String[][] noMirrorCalcItemsArr;
		
		/** Side check box for all symmetric objects. */
		private PlugInMuscleColorButtonPanel[][] mirrorCheckCalcItemsArr;
		
		/** Side check box for all non-symmetric objects. */
		private PlugInMuscleColorButtonPanel[][] noMirrorCheckCalcItemsArr;
		
		/** Buttons for all symmetric objects. */
		private JButton[][] mirrorButtonCalcItemsArr;
		
		/** Buttons for all non-symmetric objects. */
		private JButton[][] noMirrorButtonCalcItemsArr;
		
		/** Check box for whether slices sould be shown when outputting a VOI to  MIPAV's output panel */
		private JCheckBox showPerSliceCalc;
		
		/** Combo box for selecting output units */
	    private JComboBox outputUnits;
	    
	    /** Stores the last output units */
	    private String currentOutputUnits;
		
		/**Seed for random color chooser for VOIs that have not had a color assigned to them. */
		private int colorChoice = 0;
		
		/**Text output generated after hitting SAVE*/
		private CustomOutput ucsdOutput = new CustomOutput();
		
		/**Whether the custom lookup table is being displayed*/
		private boolean lutOn = false;
		
		/**A mapping of names to color panels for easy referencing. */
		private TreeMap<String,PlugInMuscleColorButtonPanel> checkBoxLocationTree;
	
		/**
		 * Constructor, note is called at beginning of program, so mirrorArr and noMirrorArr
		 * may be used in threaded calculations.
		 * 
		 * @param theParentFrame
		 * @param mirrorArr
		 * @param noMirrorArr
		 */
		public AnalysisDialogPrompt(PlugInMuscleImageDisplay theParentFrame, String[][] mirrorArr, String[][] noMirrorArr) {
	        super(theParentFrame, "Analysis");
	        
	        setButtons(buttonStringList);

	        //by looking at calculations up here, this process can be done independent
	        //of the multiple symmetries that may exist within a given body.
	        this.noMirrorCalcItemsArr = getCalcItems(noMirrorArr);
	        this.mirrorCalcItemsArr = getCalcItems(mirrorArr);
	        
	        checkBoxLocationTree = new TreeMap<String, PlugInMuscleColorButtonPanel>();

	        colorChoice = 0;
	        
	        initDialog();
	    }
		
		/**
		 * Implementing abstract method to handle calculating buttons
		 */
		public void actionPerformed(ActionEvent e) {
			ViewUserInterface.getReference().getMessageFrame().append("Caught 2: "+e.getActionCommand()+"\n", ViewJFrameMessage.DEBUG);
			String command = e.getActionCommand();
			if(e.getSource() instanceof PlugInMuscleColorButton) {
            	PlugInMuscleColorButton obj = ((PlugInMuscleColorButton)e.getSource());
		    	if (obj.getColorIcon().getColor() != Color.BLACK) {
            		VOIVector vec = getActiveImage().getVOIs();
            		for(int i=0; i<vec.size(); i++) {
            			vec.get(i).setAllActive(false);
	            		if(vec.get(i).getName().equals(obj.getVOIName())) {
	            			vec.get(i).setAllActive(true);
	            		}
            		}
            		updateImages(true);
            		
            		getComponentImage().getVOIHandler().showColorDialog();
            	}
		    } else if (command.equals(OUTPUT)) {
	        	processCalculations(false, false);
	        } else if (command.equals(SELECT_ALL)) { 
	        	pressAvailableButtons();
	        } else if (command.equals(SAVE)) {
	        	setVisible(false);
	        	boolean lutBuffer = lutOn;
	        	getActiveImage().getParentFrame().requestFocus();
	        	
	        	processCalculations(true, true);
	        	//note analysis turns off LUT automatically
	        	if(lutOn = lutBuffer) {
	        		loadLUT();
	        	}
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
	        	if(imageType.equals(ImageType.Thigh))
	        		MipavUtil.showHelp("MS00040");
	        	else //image is of type abdomen
	        		MipavUtil.showHelp("MS00080");
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
	        			((JButton)e.getSource()).setSelected(false);
	        		}
	        	if(!exists) {
	        		VOI rec = voiBuffer.get(text);
	        		Color c = null;
	        		if((c = voiBuffer.get(rec.getName()).getColor()).equals(PlugInSelectableVOI.INVALID_COLOR) && 
	        				(c = hasColor(rec)).equals(PlugInSelectableVOI.INVALID_COLOR)) {
	            		c = colorPick[colorChoice++ % colorPick.length];
	            	}
	        		rec.removeVOIListener(checkBoxLocationTree.get(text).getColorButton());
	        		rec.addVOIListener(checkBoxLocationTree.get(text).getColorButton());
	        		
	            	rec.setColor(c);
	        		
	        		if(lutOn && getZeroStatus(rec.getName())) {
	                	rec.setDisplayMode(VOI.SOLID);
	                	rec.setOpacity((float)0.7);
	                }
	        		((JButton)e.getSource()).setSelected(true);
	        		getActiveImage().registerVOI(rec);
	        	}	
	        	updateImages(true);
	        }
		}

		public boolean calcOutputSuccess() {
			return ucsdOutput.isSuccess();
		}

		/**
		 * Enables buttons that rely on calculating being completed.
		 */
		public void enableCalcOutput() {
			for(int i=0; i<buttonGroup.length; i++) {
		    	if(buttonGroup[i].getText().equals(OUTPUT)) {
		    		buttonGroup[i].setEnabled(true);
		    	} else if(buttonGroup[i].getText().equals(SELECT_ALL)) {
		    		buttonGroup[i].setEnabled(true);
		    	} else if(buttonGroup[i].getText().equals(SAVE)) {
		    		buttonGroup[i].setEnabled(true);
		    	}
		    }
		}
		
		/**
		 * Returns the units that this plugin will output in.
		 * 
		 */
		public String getSelectedOutput() {
			return outputUnits.getSelectedItem().toString();
		}
		
		/**
		 * Returns whether the output button should show all slice calculations of the selected VOI.
		 */
		public boolean showPerSliceCalc() {
			return showPerSliceCalc.isSelected();
		}
		
		/**
		 * Presses all enabled buttons.  Boolean selectMode has following actions:
		 * when true - any available unselected buttons are pressed
		 * when false - any available selected buttons are pressed
		 */
		public void pressAvailableButtons() {
			for(int i=0; i<mirrorButtonCalcItemsArr.length; i++) {
				for(int j=0; j<mirrorButtonCalcItemsArr[i].length; j++) {
					if(mirrorButtonCalcItemsArr[i][j].isEnabled() && !mirrorButtonCalcItemsArr[i][j].isSelected()) {
						mirrorButtonCalcItemsArr[i][j].doClick();
						mirrorButtonCalcItemsArr[i][j].setSelected(true);
					}	
				}
			}
			for(int i=0; i<noMirrorButtonCalcItemsArr.length; i++) {
				for(int j=0; j<noMirrorButtonCalcItemsArr[i].length; j++) {
					if(noMirrorButtonCalcItemsArr[i][j].isEnabled() && !noMirrorButtonCalcItemsArr[i][j].isSelected()) {
						noMirrorButtonCalcItemsArr[i][j].doClick();
						noMirrorButtonCalcItemsArr[i][j].setSelected(true);
					}	
				}
			}
		}

		/**
		 * Implemented abstract method for dealing with slice changes, currently sets buttons of VOIs that
		 * have curves in the given slice to ENABLED, disables buttons where a VOI has no curve on that slice.
		 */
		public void setSlice(int slice) {
			if (checkBoxLocationTree != null) {
				Set<String> keySet = checkBoxLocationTree.keySet();
				Iterator<String> it = keySet.iterator();
				while(it.hasNext()) {
					//Stays black until pressed.
					String next = it.next();
					
					checkBoxLocationTree.get(next).setColor(Color.BLACK);
					checkBoxLocationTree.get(next).repaint();
				}
			}
			boolean voiExists = false;
			String buttonName = new String();
			for(int index=0; index<mirrorButtonCalcItemsArr.length; index++) {
				for(int i=0; i<mirrorButtonCalcItemsArr[index].length; i++) {
					mirrorButtonCalcItemsArr[index][i].setEnabled(voiExists = voiExists(mirrorButtonCalcItemsArr[index][i].getText(), slice));
					buttonName = mirrorButtonCalcItemsArr[index][i].getText();
					if(!voiExists) {
						checkBoxLocationTree.get(buttonName).setColor(Color.BLACK);
						checkBoxLocationTree.get(buttonName).repaint();
					} else if (voiExists && mirrorButtonCalcItemsArr[index][i].isSelected()){
						checkBoxLocationTree.get(buttonName).setColor(voiBuffer.get(buttonName).getColor());
						checkBoxLocationTree.get(buttonName).repaint();
					}
					mirrorButtonCalcItemsArr[index][i].setForeground(Color.BLACK);
					if(voiExists && voiBuffer.get(mirrorButtonCalcItemsArr[index][i].getText()).isComputerGenerated())
						mirrorButtonCalcItemsArr[index][i].setForeground(Color.RED);
				}
				for(int i=0; i<noMirrorButtonCalcItemsArr[index].length; i++) {
					noMirrorButtonCalcItemsArr[index][i].setEnabled(voiExists = voiExists(noMirrorButtonCalcItemsArr[index][i].getText(), slice));
					buttonName = noMirrorButtonCalcItemsArr[index][i].getText();
					if(!voiExists) {
						checkBoxLocationTree.get(buttonName).setColor(Color.BLACK);
						checkBoxLocationTree.get(buttonName).repaint();
					} else if (voiExists && noMirrorButtonCalcItemsArr[index][i].isSelected()){
						checkBoxLocationTree.get(buttonName).setColor(voiBuffer.get(buttonName).getColor());
						checkBoxLocationTree.get(buttonName).repaint();
					}
					noMirrorButtonCalcItemsArr[index][i].setForeground(Color.BLACK);
					if(voiExists && voiBuffer.get(noMirrorButtonCalcItemsArr[index][i].getText()).isComputerGenerated())
						noMirrorButtonCalcItemsArr[index][i].setForeground(Color.RED);
				}
			}
		}
		
		public void setUpDialog() {
			for(int i=0; i<mirrorButtonCalcItemsArr.length; i++) {
				for(int j=0; j<mirrorButtonCalcItemsArr[i].length; j++) {
					mirrorCheckCalcItemsArr[i][j].setColor(Color.BLACK);
					if(mirrorButtonCalcItemsArr[i][j].isEnabled()) {
						mirrorButtonCalcItemsArr[i][j].setSelected(false);
					}	
				}
			}
			for(int i=0; i<noMirrorButtonCalcItemsArr.length; i++) {
				for(int j=0; j<noMirrorButtonCalcItemsArr[i].length; j++) {
					noMirrorCheckCalcItemsArr[i][j].setColor(Color.BLACK);
					if(noMirrorButtonCalcItemsArr[i][j].isEnabled()) {
						noMirrorButtonCalcItemsArr[i][j].setSelected(false);
					}	
				}
			}
		}

		/**
		 * Initializes the dialog box.
		 */    
		protected void initDialog() {
		    setForeground(Color.black);
		    
		    JPanel instructionPanel = initInstructionPanel();
		    
		    JScrollPane mirrorPanel[] = new JScrollPane[mirrorCalcItemsArr.length];
		    
		    JPanel mainPanel = new JPanel();
		    mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		    
		    mainPanel.add(instructionPanel);
		
		    mirrorCheckCalcItemsArr = new PlugInMuscleColorButtonPanel[mirrorCalcItemsArr.length][];
		    mirrorButtonCalcItemsArr = new JButton[mirrorCalcItemsArr.length][];
		    
		    noMirrorCheckCalcItemsArr = new PlugInMuscleColorButtonPanel[noMirrorCalcItemsArr.length][];
		    noMirrorButtonCalcItemsArr = new JButton[noMirrorCalcItemsArr.length][];
		
		    String title = "";
		    //guaranteed (for now) that mirrorArr.length = noMirrorArr.length
		    for(int i=0; i<mirrorCalcItemsArr.length; i++) {
		    	
		    	JPanel subPanel = initSymmetricalObjects(i);
		    	initNonSymmetricalObjects(subPanel, i);
		    	
		    	mirrorPanel[i] = new JScrollPane(subPanel, 
		    										ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER,
		        									ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		    	
		    	mirrorPanel[i].setForeground(Color.black);
		        String vowel = ((title = titles[i]).charAt(0) == 'a' || 
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
		    	if(buttonGroup[i].getText().equals(TOGGLE_LUT)) {
		    		buttonGroup[i].setText("Show LUT");
		    	} else if(buttonGroup[i].getText().equals(OUTPUT)) {
		    		buttonGroup[i].setEnabled(false);
		    	} else if(buttonGroup[i].getText().equals(SELECT_ALL)) {
		    		buttonGroup[i].setEnabled(false);
		    	} else if(buttonGroup[i].getText().equals(SAVE)) {
		    		buttonGroup[i].setEnabled(false);
		    	}
		    }
		    
		    
		    
		    add(mainPanel, BorderLayout.CENTER);
		    
		}

		/**
		 * Gets all items (symmetric and non-symmetric) that are calulated.
		 */
		private String[][] getCalcItems(String[][] objectArr) {
			String[][] resultArr = new String[objectArr.length][];
			String tempStr = "";
			for(int i=0; i<objectArr.length; i++) {
				ArrayList<String> tempObj = new ArrayList<String>();
				for(int j=0; j<objectArr[i].length; j++) {
					if(calcTree.get(tempStr = objectArr[i][j]).equals(true)) {
						tempObj.add(tempStr);
					}
				}
				resultArr[i] = new String[tempObj.size()];
				for(int j=0; j<resultArr[i].length; j++) {
					resultArr[i][j] = tempObj.get(j);
				}
			}
			return resultArr;
		}
		
		/**
		 * Initializes the instruction panel.
		 */
		private JPanel initInstructionPanel() {
	        GridBagConstraints gbc = new GridBagConstraints();
	        gbc.anchor = GridBagConstraints.NORTHWEST;
	        gbc.fill = GridBagConstraints.HORIZONTAL;
	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.weightx = 0;
	        gbc.weighty = 0;
	        
	        JPanel instructionPanel = new JPanel(new GridBagLayout());
	        instructionPanel.setForeground(Color.black);
	        instructionPanel.setBorder(MipavUtil.buildTitledBorder("Instructions"));
	        String message = "<html>1) Click on the muscle you would like to view.<br>"+
	        					"2) Check boxes indicate whether a particular VOI exists.";
	        instructionLabel = new JLabel(message);
	        instructionLabel.setFont(MipavUtil.font12);
	        instructionPanel.add(instructionLabel, gbc);
	 
	        JPanel optionPanel = new JPanel();
		    
		    if(multipleSlices) {  //volumes never shown when 2D, even if slice thickness specified
			    gbc.gridy = 1;
			    gbc.weightx = 1;
		    	gbc.fill = GridBagConstraints.CENTER;
			    showPerSliceCalc = new JCheckBox("Show per-slice volume calculations");
			    showPerSliceCalc.setFont(MipavUtil.font12);
			    showPerSliceCalc.setBorder(new EmptyBorder(10, 0, 0, 0));
		    	JPanel blankPanel = new JPanel();
		    	blankPanel.add(showPerSliceCalc);
			    instructionPanel.add(blankPanel, gbc);
		    }
		    
		    JLabel unitLabel = new JLabel("Select output units: ");
	        unitLabel.setForeground(Color.black);
	        unitLabel.setBorder(new EmptyBorder(0, 0, 0, 10));
	        unitLabel.setFont(MipavUtil.font12);
	        optionPanel.add(unitLabel);
		    
		    int measure = imageA.getUnitsOfMeasure()[0];
		    String measureText = "Centimeters"; //default output is in cm by user request
	        int[] allSameMeasure = FileInfoBase.getAllSameDimUnits(measure);
	        String[] unitArr = new String[allSameMeasure.length];
	        for(int i=0; i<allSameMeasure.length; i++) {
	        	unitArr[i] = FileInfoBase.getUnitsOfMeasureStr(allSameMeasure[i]);
	        }
	        
	        outputUnits = new JComboBox(unitArr);
	        outputUnits.setFont(MipavUtil.font12);
	        outputUnits.setSelectedItem(measureText);
	        currentOutputUnits = measureText;
	        outputUnits.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					if(!currentOutputUnits.equals(outputUnits.getSelectedItem())) {
						int option = JOptionPane.showConfirmDialog(getParent(), 
								"Changing the output units requires recalculation, would you like to continue?", 
								"Output units", JOptionPane.YES_NO_OPTION);
						if(option == JOptionPane.YES_OPTION) {
							forceVoiRecalc();
							currentOutputUnits = outputUnits.getSelectedItem().toString();
						} else {
							outputUnits.setSelectedItem(currentOutputUnits);
						}
					}
					ViewUserInterface.getReference().getMessageFrame().append(e.paramString()+"\n", ViewJFrameMessage.DEBUG);
				}
	        });
	        optionPanel.add(outputUnits);
		    
	        gbc.gridy = gbc.gridy+1;
	        gbc.weightx = 1;
		    instructionPanel.add(optionPanel, gbc);
	        
	        return instructionPanel;
	    }
		
		/**
		 * Initializes the symmetric buttons from a particular pane.
		 */
		private JPanel initSymmetricalObjects(int index) {

            mirrorCheckCalcItemsArr[index] = new PlugInMuscleColorButtonPanel[mirrorCalcItemsArr[index].length * 2];
            mirrorButtonCalcItemsArr[index] = new JButton[mirrorCalcItemsArr[index].length * 2];
			JPanel subPanel = new JPanel(new GridBagLayout());
            subPanel.setForeground(Color.black);

			String[] mirrorString = new String[mirrorCalcItemsArr[index].length * 2];
	        
	        for(int i=0; i<mirrorCalcItemsArr[index].length * 2; i++) {
	            String symmetry1 = "", symmetry2 = "";
	            if(symmetry.equals(Symmetry.LEFT_RIGHT)) {
	                symmetry1 = "Left ";
	                symmetry2 = "Right ";
	            } else if(symmetry.equals(Symmetry.TOP_BOTTOM)) {
	                symmetry1 = "Top ";
	                symmetry2 = "Bottom ";
	            }
	            
	            mirrorString[i] = (i % 2) == 0 ? new String(symmetry1+mirrorCalcItemsArr[index][i/2]) : 
	                                                        new String(symmetry2+mirrorCalcItemsArr[index][i/2]);
	        }
	        
	        GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1;
            gbc.weighty = 0;
            gbc.gridx = 0;
            gbc.gridy = 0;
            
            for(int i=0; i<mirrorCalcItemsArr[index].length * 2; i++) {
                               
            	mirrorCheckCalcItemsArr[index][i] = new PlugInMuscleColorButtonPanel(Color.BLACK, mirrorString[i], this);
                
                checkBoxLocationTree.put(mirrorString[i], mirrorCheckCalcItemsArr[index][i]);
                
                mirrorButtonCalcItemsArr[index][i] = new JButton(mirrorString[i]);
                mirrorButtonCalcItemsArr[index][i].setEnabled(voiExists(mirrorString[i]));
                mirrorButtonCalcItemsArr[index][i].setFont(MipavUtil.font12B);
                mirrorButtonCalcItemsArr[index][i].setActionCommand(LOAD_VOI);
                mirrorButtonCalcItemsArr[index][i].addActionListener(this);
                
                if(i != 0 && i % 2 == 0) {
                    gbc.gridy++;
                    gbc.gridx = 0;
                }
                gbc.weightx = 0;
                gbc.insets = new Insets(0, 10, 0, 0);
                subPanel.add(mirrorCheckCalcItemsArr[index][i], gbc);
                gbc.insets = new Insets(0, 0, 0, 0);
                gbc.gridx++;
                gbc.weightx = 1;
                subPanel.add(mirrorButtonCalcItemsArr[index][i], gbc);
                gbc.gridx++;
                
            }          
	        
	        return subPanel;         
	    }
	    
		/**
		 * Initializes the non-symmetric buttons from a particular pane.
		 */
	    private JPanel initNonSymmetricalObjects(JPanel subPanel, int index) {
      
	    	noMirrorCheckCalcItemsArr[index] = new PlugInMuscleColorButtonPanel[noMirrorCalcItemsArr[index].length];
            noMirrorButtonCalcItemsArr[index] = new JButton[noMirrorCalcItemsArr[index].length];
	    	
	    	GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1;
            gbc.weighty = 0;
            gbc.gridx = 0;
            gbc.gridy = noMirrorCalcItemsArr[index].length;
            
            for(int i=0; i<noMirrorCalcItemsArr[index].length; i++) {
                               
            	noMirrorCheckCalcItemsArr[index][i] = new PlugInMuscleColorButtonPanel(Color.BLACK, noMirrorCalcItemsArr[index][i], this);
                
                checkBoxLocationTree.put(noMirrorCalcItemsArr[index][i], noMirrorCheckCalcItemsArr[index][i]);
                
                noMirrorButtonCalcItemsArr[index][i] = new JButton(noMirrorCalcItemsArr[index][i]);
                noMirrorButtonCalcItemsArr[index][i].setEnabled(voiExists(noMirrorCalcItemsArr[index][i]));
                noMirrorButtonCalcItemsArr[index][i].setFont(MipavUtil.font12B);
                noMirrorButtonCalcItemsArr[index][i].setActionCommand(LOAD_VOI);
                noMirrorButtonCalcItemsArr[index][i].addActionListener(this);
                
                if(i != 0 && i % 2 == 0) {
                    gbc.gridy++;
                    gbc.gridx = 0;
                }
                gbc.weightx = 0;
                gbc.insets = new Insets(0, 10, 0, 0);
                subPanel.add(noMirrorCheckCalcItemsArr[index][i], gbc);
                gbc.insets = new Insets(0, 0, 0, 0);
                gbc.gridx++;
                gbc.weightx = 1;
                subPanel.add(noMirrorButtonCalcItemsArr[index][i], gbc);
                gbc.gridx++;   
            }          
	        
	        return subPanel;
	    }
	    
	    /**
		 * Loads the CT Thigh specific lut
		 */
		private void loadLUT() {
			float min = (float)getActiveImage().getMin();
			float max = (float)getActiveImage().getMax();
			
			TransferFunction transfer = new TransferFunction();
			transfer.addPoint(new Vector2f(min, 255));
			
			//fat = blue
			transfer.addPoint(new Vector2f(-190, 255));
			transfer.addPoint(new Vector2f(-190, 254));
			transfer.addPoint(new Vector2f(-30, 254));
			
			//partial = white
			transfer.addPoint(new Vector2f(-30, 5));
			transfer.addPoint(new Vector2f(0, 5));
			
			//muscle = red
			transfer.addPoint(new Vector2f(0, 0));
			transfer.addPoint(new Vector2f(100, 0));
			
			//rest is black
			transfer.addPoint(new Vector2f(100, 255));
			transfer.addPoint(new Vector2f(max, 255));
			
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

		/**
		 * Does calculations on each (or all) of the various areas, and either saves them
		 * to disk in a PDF or outputs in the Output-Data tab
		 * @param all whether to calculate all (true if PDF)
		 * @param doSave whether to save the output (and screen grabs) to a pdf
		 */
		private void processCalculations(boolean all, boolean doSave) {
			if(calcGroup.activeCount() > 0) {
				//TODO: Use ExecutorService now that 1.6
				//Note that since the buttons are disabled, this could only happen by being
				//directly called in the code
				Thread[] activeThread = new Thread[calcGroup.activeCount()];
				calcGroup.enumerate(activeThread);
				String activeStr = new String();
				for(int i=0; i<calcGroup.activeCount(); i++) {
					activeStr += activeThread[i].getName()+"\n";
				}
				MipavUtil.displayError("Still processing calculations.  Please wait for the\nfollowing "+
										"calculations to complete:\n"+activeStr);
				return;
			}
		
			boolean pdfCreated = false;
			
			String textFileDir = null, textFileName = null, pdfFileDir = null, pdfFileName = null;
			if(doSave) {
				FileLocation fl = new FileLocation(muscleFrame);
				
				if(!fl.doCalc()) {  //user chose to not save files.
					return;
				}
				
				textFileDir = fl.getTextFileDir();
				textFileName = fl.getTextFileName();
				pdfFileDir = fl.getPdfFileDir();
				pdfFileName = fl.getPdfFileName();
			}
			
			//if PDF hasnt been created and we're saving, create it now
			if (doSave && !pdfCreated) {
				PDFcreate(pdfFileDir, pdfFileName);
				pdfCreated = true;
			}
			Iterator<String> itr;
			if(all) {
				itr = voiBuffer.keySet().iterator();
			} else {
				ArrayList<String> totalList = new ArrayList<String>(), subList = new ArrayList<String>();
				for (int listNum = 0; listNum < mirrorButtonCalcItemsArr.length; listNum++, subList = new ArrayList<String>())  {   	
					for(int i=0; i<mirrorButtonCalcItemsArr[listNum].length; i++)  {
						if(mirrorCheckCalcItemsArr[listNum][i].isSelected()) {
							subList.add(mirrorButtonCalcItemsArr[listNum][i].getText());
						}
					}
					totalList.addAll(subList);
		    	}
				for (int listNum = 0; listNum < noMirrorButtonCalcItemsArr.length; listNum++, subList = new ArrayList<String>())  {   	
					for(int i=0; i<noMirrorButtonCalcItemsArr[listNum].length; i++) {
						if(noMirrorCheckCalcItemsArr[listNum][i].isSelected()) {
							subList.add(noMirrorButtonCalcItemsArr[listNum][i].getText());
						}
					}
					totalList.addAll(subList);
		    	}
				itr = totalList.iterator();
			}
				
			
			while(itr.hasNext()) {
				Object itrObj = itr.next();
				double totalAreaCount = 0, fatArea = 0, leanArea = 0;//, partialArea = 0;
				double meanFatH = 0, meanLeanH = 0, meanTotalH = 0;
				//pixels -> cm^2\
				PlugInSelectableVOI temp;
				if((temp = voiBuffer.get(itrObj)) != null && temp.getCalcEligible()) {
					
					totalAreaCount = temp.getTotalArea();
					fatArea = temp.getFatArea();
					leanArea = temp.getLeanArea();
					meanFatH = temp.getMeanFatH();
					meanLeanH = temp.getMeanLeanH();
					meanTotalH = temp.getMeanTotalH();
					
					ViewUserInterface.getReference().getMessageFrame().append("Compare areas of "+temp.getName()+":\tcount: "+totalAreaCount+"\n", ViewJFrameMessage.DEBUG);
					
					if (doSave) {
						PDFadd((String)itrObj, fatArea, leanArea, totalAreaCount, meanFatH, meanLeanH, meanTotalH, wholeTable);
						if(multipleSlices) {
							for(int i=0; i<getActiveImage().getExtents()[2]; i++) {
								totalAreaCount = temp.getTotalArea(i);
								fatArea = temp.getFatArea(i);
								leanArea = temp.getLeanArea(i);
								meanFatH = temp.getMeanFatH(i);
								meanLeanH = temp.getMeanLeanH(i);
								meanTotalH = temp.getMeanTotalH(i);
								
								PDFadd((String)itrObj, fatArea, leanArea, totalAreaCount, meanFatH, meanLeanH, meanTotalH, sliceTable[i]);
							}
						}
					} else {
						DecimalFormat dec = new DecimalFormat("0.##");
						StringBuffer build = new StringBuffer();
						String type = multipleSlices ? "Volume" : "Area";
						
						build.append(itrObj).append(" calculations:\n"+"Fat ").append(type).append(": ").append(dec.format(fatArea)).append(
						"\t\t\t\tMean H: ").append(dec.format(meanFatH)+"\nLean ").append(type).append(": ").append(dec.format(leanArea)).append(
						"\t\t\tMean H: ").append(dec.format(meanLeanH)+"\nTotal ").append(type).append(": ").append(dec.format(totalAreaCount)).append(
						"\t\t\tMean H: ").append(dec.format(meanTotalH) + "\n\n");
						
						if(multipleSlices && showPerSliceCalc.isSelected()) {
							for(int i=0; i<getActiveImage().getExtents()[2]; i++) {
								build.append("Slice ").append(i).append(":\n").append("Fat Area: ").append(dec.format(temp.getFatArea(i))).append(
								"\t\t\t\tMean H: ").append(dec.format(temp.getMeanFatH(i))).append("\nLean Area: ").append(dec.format(temp.getLeanArea(i))).append(
								"\t\t\tMean H: ").append(dec.format(temp.getMeanLeanH(i))).append("\nTotal Area: ").append(dec.format(temp.getTotalArea(i))).append(
								"\t\t\tMean H: ").append(dec.format(temp.getMeanTotalH(i))).append("\n\n");
							}
						}
					
						ViewUserInterface.getReference().getMessageFrame().append(build.toString(), ViewJFrameMessage.DATA);
					}	
				}
			}
		
			if (doSave) {
				ucsdOutput.setFileDir(textFileDir);
				ucsdOutput.setFileName(textFileName);
				Thread output = new Thread(ucsdOutput);
		    	output.start();
				
				//now load all VOIs at once:
				ArrayList<String> totalList = new ArrayList<String>(), subList = new ArrayList<String>();
				for (int listNum = 0; listNum < mirrorButtonCalcItemsArr.length; listNum++, subList = new ArrayList<String>())  {   	
					for(int i=0; i<mirrorButtonCalcItemsArr[listNum].length; i++) {
						if(mirrorButtonCalcItemsArr[listNum][i].isEnabled()) {
							subList.add(mirrorButtonCalcItemsArr[listNum][i].getText());
						}
					}
					totalList.addAll(subList);
		    	}
				for (int listNum = 0; listNum < noMirrorButtonCalcItemsArr.length; listNum++, subList = new ArrayList<String>())  {   	
					for(int i=0; i<noMirrorButtonCalcItemsArr[listNum].length; i++) {
						if(noMirrorButtonCalcItemsArr[listNum][i].isEnabled()) {
							subList.add(noMirrorButtonCalcItemsArr[listNum][i].getText());
						}
					}
					totalList.addAll(subList);
		    	}
				String[] allStrings = new String[totalList.size()];
				for(int i=0; i<totalList.size(); i++) {
					allStrings[i] = totalList.get(i) + ".xml";
				}
				 
				loadVOIs(allStrings, .4);
			
				//loadLUT();
				updateImages(true);
						
				java.awt.Image edgeImage = captureImage();
				
				loadVOIs(new String[] {}, 0);
				loadLUT();
				java.awt.Image qaImage = captureImage();
				removeLUT();
				PDFclose(edgeImage, qaImage);
				
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

		/**
		 * Class to set file locations/names of PDF and text files
		 * 
		 * @author senseneyj
		 *
		 */
		private class FileLocation extends JDialogBase implements ActionListener {
			
			private JButton browsePDF, browseText;

			private JTextField textDirField, textNameField;
			
			private JTextField pdfDirField, pdfNameField;
			
			private String textFileDir, textFileName;
			
			private String pdfFileDir, pdfFileName;
			
			private boolean doCalc = false;
			
			public FileLocation(Frame theParentFrame) {
				super(theParentFrame, true);
				
				if(imageDir != null) {
					textFileDir = imageDir;
					pdfFileDir = imageDir;
				} else {
					textFileDir = System.getProperty("user.home");
					pdfFileDir = System.getProperty("user.home");
				}
				
				textFileName = "Text_Report.txt";
				pdfFileName = "PDF_Report.pdf";
				
				init();
			}

			private void init() {
				setForeground(Color.black);
		        setTitle("Select name and location of text and PDF data files");

		        GridBagConstraints gbc = new GridBagConstraints();
		        gbc.gridwidth = 1;
		        gbc.gridheight = 1;
		        gbc.anchor = GridBagConstraints.WEST;
		        gbc.weightx = 0;
		        gbc.insets = new Insets(3, 3, 3, 3);
		        gbc.fill = GridBagConstraints.HORIZONTAL;
		        gbc.gridx = 0;
		        gbc.gridy = 0;

		        JPanel mainPanel = new JPanel(new GridBagLayout());
		        mainPanel.setForeground(Color.black);
		        mainPanel.setBorder(buildTitledBorder("Text and PDF File Locations"));

		        mainPanel.add(new JLabel("PDF File Directory:"), gbc);
		        
		        pdfDirField = new JTextField(pdfFileDir);
		        gbc.gridx = 1;
		        gbc.gridy = 0;
		        gbc.weightx = 1;
		        mainPanel.add(pdfDirField, gbc);
		        
		        browsePDF = new JButton("Browse");
		        browsePDF.addActionListener(this);
		        gbc.gridx = 2;
		        gbc.gridy = 0;
		        gbc.weightx  = 0;
		        mainPanel.add(browsePDF, gbc);
		        
		        gbc.gridx = 3;
		        gbc.gridy = 0;
		        mainPanel.add(new JLabel("PDF File Name:"), gbc);

		        pdfNameField = new JTextField(pdfFileName);
		        gbc.gridx = 4;
		        gbc.gridy = 0;
		        gbc.weightx = 1;
		        mainPanel.add(pdfNameField, gbc);
		        
		        gbc.gridx = 0;
		        gbc.gridy = 1;
		        gbc.weightx  = 0;
		        mainPanel.add(new JLabel("Text File Directory:"), gbc);
		        
		        textDirField = new JTextField(textFileDir);
		        gbc.gridx = 1;
		        gbc.gridy = 1;
		        gbc.weightx = 1;
		        mainPanel.add(textDirField, gbc);

		        browseText = new JButton("Browse");
		        browseText.addActionListener(this);
		        gbc.gridx = 2;
		        gbc.gridy = 1;
		        gbc.weightx  = 0;
		        mainPanel.add(browseText, gbc);
		        
		        gbc.gridx = 3;
		        gbc.gridy = 1;
		        mainPanel.add(new JLabel("Text File Name:"), gbc);

		        textNameField = new JTextField(textFileName);
		        gbc.gridx = 4;
		        gbc.gridy = 1;
		        gbc.weightx = 1;
		        mainPanel.add(textNameField, gbc);
		        
		        getContentPane().add(mainPanel, BorderLayout.CENTER);

		        // Build the Panel that holds the OK and CANCEL Buttons
		        JPanel OKCancelPanel = new JPanel();

		        // size and place the OK button
		        buildOKButton();
		        OKCancelPanel.add(OKButton, BorderLayout.WEST);

		        // size and place the CANCEL button
		        buildCancelButton();
		        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
		        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

		        pack();
		        setVisible(true);
		        setResizable(false);
		        System.gc();
			}

			public String getTextFileDir() {
				return textFileDir;
			}

			public String getTextFileName() {
				return textFileName;
			}

			public String getPdfFileDir() {
				return pdfFileDir;
			}

			public String getPdfFileName() {
				return pdfFileName;
			}
			
			public boolean doCalc() {
				return doCalc;
			}

			public void actionPerformed(ActionEvent e) {
				if(e.getSource().equals(OKButton)) {
					textFileDir = textDirField.getText();
					textFileName = textNameField.getText();
					
					pdfFileDir = pdfDirField.getText();
					pdfFileName = pdfNameField.getText();
					
					setVisible(false);
					//create folders if necessary (may have been manually entered)
					if(!new File(textFileDir).exists()) {
						new File(textFileDir).mkdirs();
					}
					if(!new File(pdfFileDir).exists()) {
						new File(pdfFileDir).mkdirs();
					}
					//check that user wants to overwrite any existing files
					boolean doGo = checkExists(textFileDir + File.separator + textFileName);
					if(doGo) {
						doGo = checkExists(pdfFileDir + File.separator + pdfFileName);
					}
					
					if(doGo) {
						doCalc = true;
						dispose();
					} else { //don't calc if user didn't want one of the files to be overwritten.
						doCalc = false;
						setVisible(true);
					}
				} else if(e.getSource().equals(cancelButton)) {
					dispose();
					doCalc = false;
				} else if(e.getSource().equals(browsePDF)) {
					pdfFileDir = setDirLoc(pdfFileDir);
					pdfDirField.setText(pdfFileDir);
				} else if(e.getSource().equals(browseText)) {
					textFileDir = setDirLoc(textFileDir);
					textDirField.setText(textFileDir);
				}
			}
			
			private boolean checkExists(String fileName) {
				String message = "<html>The file "+fileName+" already exists.<br>Are you sure you want to overwrite this file?</html>";
				if(new File(fileName).exists()) {
					int val = JOptionPane.showConfirmDialog(this, message, "File already exists", JOptionPane.YES_NO_OPTION);

					if(val == JOptionPane.YES_OPTION) {
						return true;
					} else {
						return false;
					}
				} else {
					return true;
				}
			}
			
			private String setDirLoc(String fileDir) {
				//Select file directory and names, create if necessary
				JFileChooser chooser = new JFileChooser();
				if(new File(fileDir).exists()) {
					chooser.setCurrentDirectory(new File(fileDir));
				} else if(new File(imageDir).exists()) {
	            	chooser.setCurrentDirectory(new File(imageDir));
	            } else {
	                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	            }
				String type = "";
				if(fileDir.equals(pdfFileDir)) {
					type = "PDF";
				} else if(fileDir.equals(textFileDir)) {
					type = "text";
				}
				
				chooser.setDialogTitle("Select a directory where the "+type+" data file will be saved");
	            chooser.setMultiSelectionEnabled(false);
	            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

	            if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
	                fileDir = chooser.getSelectedFile().toString();
	                if(!chooser.getSelectedFile().exists()) {
	                	chooser.getSelectedFile().mkdirs();
	                }                
	            } else {
	            	//user chose to not save calculations to PDF/text
	            }
	            
	            return fileDir;
			}
		}
		
		

		/**
		 * Removes the CT Thigh specific lut
		 */
		private void removeLUT() {
			
			ctMode(getActiveImage(), -175, 275);
			
			getLUTa().makeGrayTransferFunctions();
			getLUTa().makeLUT(256);
			
			VOIVector vec = getActiveImage().getVOIs();
			for(int i=0; i<vec.size(); i++) {
				vec.get(i).setDisplayMode(VOI.CONTOUR);
			}
			
			updateImages(true);
			
			lutOn = false;
		}

		/**
		 * Generates tab-delimited text output.
		 * 
		 * @author senseneyj
		 *
		 */
		private class CustomOutput implements Runnable {
			
			/** Whether this Runnable has completed*/
			private boolean done = false;
			
			/** Whether the Runnable successfully wrote the file*/
			private boolean success = false;
			
			/** File directory to save to **/
			private String fileDir = imageDir;
			
			/**Name of text file to save**/
			private String fileName;
			
			/**
			 * Whether the output has finished.
			 */
			public boolean isFinished() {
				return done;
			}
			
			/**
			 * Whether the output completed successfully
			 */
			public boolean isSuccess() {
				return success;
			}
			
			public void setFileDir(String fileDir) {
				this.fileDir = fileDir;
			}
			
			public void setFileName(String fileName) {
				this.fileName = fileName;
			}

			/**
			 * Produces output into text file in NIA_Seg
			 */
			public void run() {
				long time = System.currentTimeMillis();
				ArrayList<PlugInSelectableVOI> calcList = new ArrayList<PlugInSelectableVOI>();
				Iterator<String> tempItr = voiBuffer.keySet().iterator();
				PlugInSelectableVOI temp = null;
				while(tempItr.hasNext()) {
					if((temp = voiBuffer.get(tempItr.next())).getCalcEligible()) {
						calcList.add(temp);
					}
				}
				
				//set file directory
				if(!(new File(fileDir).exists())) {
					fileDir = imageDir;
				}
				
				//set file name
				if(fileName == null) {
					fileName = "Text_Report.txt";
					textFile = new File(fileDir + File.separator + fileName);
					if(textFile.exists()) {
						int i=0;
						while(textFile.exists() && i<1000) {
							fileName = "Text_Report-"+(++i)+ ".txt";
							if(i == 1000) {
								MipavUtil.displayError("Too many text documents have been created, overwriting "+fileName);
							}
							textFile = new File(fileDir + File.separator + fileName);
						}
					}
				} else {
					textFile = new File(fileDir + File.separator + fileName);
				}
				
				ViewUserInterface.getReference().getMessageFrame().append("Text path: "+textFile.getAbsolutePath()+"\n", ViewJFrameMessage.DEBUG);
				
				String[] output = assembleOutput();
				
				try {
					BufferedWriter writer = new BufferedWriter(new FileWriter(textFile));
					
					//write output
					for(int i=0; i<output.length; i++) {
						writer.write(output[i]);
						writer.newLine();
					}
					
					writer.close();
					success = true;
				} catch(IOException e) {
					System.err.println("Error creating, writing or closing ucsd file.");
					e.printStackTrace();
					success = false;
					return;
				}
				ViewUserInterface.getReference().getMessageFrame().append("Time for output: "+(System.currentTimeMillis() - time)+"\n", ViewJFrameMessage.DEBUG);
				done = true;
			}
			
			/**
			 * Assembles output for all slices of image.
			 * @return array of slice data
			 */
			private String[] assembleOutput() {
				String[] sliceStr;
				if(getActiveImage().getExtents().length > 2) {
					sliceStr = new String[getActiveImage().getExtents()[2]];
				} else {
					sliceStr = new String[1];
				}
					
				for(int i=0; i<sliceStr.length; i++) {
					sliceStr[i] = new String();
					
					//insert static elements
					FileInfoDicom fileInfo = (FileInfoDicom)getActiveImage().getFileInfo()[0];
					DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
					Date date = new Date();
					DecimalFormat dec = new DecimalFormat("0.0000");
					//patient id
					String id = (String)fileInfo.getTagTable().getValue("0010,0020");
					sliceStr[i] += (id != null ? id.trim() : "0")+"\t";
					//slice number
					sliceStr[i] += Integer.toString(i)+"\t";
					//scan date
					String dateStr = (String)fileInfo.getTagTable().getValue("0008,0020");
					sliceStr[i] += (dateStr != null ? dateStr.trim() : "0")+"\t";
					//center
					String center = (String)fileInfo.getTagTable().getValue("0008,0080");
					sliceStr[i] += (center != null ? center.trim() : "0")+"\t";
					//analysis date
					sliceStr[i] += dateFormat.format(date)+"\t";
					//analyst
					sliceStr[i] += System.getProperty("user.name")+"\t";
					//pixel size
					sliceStr[i] += dec.format(getActiveImage().getResolutions(0)[0]*.1)+"\t";
					//slice thickness (mm)
					sliceStr[i] += dec.format(getActiveImage().getResolutions(0)[2])+"\t";
					//table height (cm)
					String heightUnformat = (String)fileInfo.getTagTable().getValue("0018,1130");
					String height = heightUnformat != null ? dec.format(Double.valueOf(heightUnformat.trim())) : "0";
					sliceStr[i] += height+"\t";
					
					//insertCalculations
					PlugInSelectableVOI temp = null;
					ArrayList<PlugInSelectableVOI> calcItems = new ArrayList<PlugInSelectableVOI>(voiBuffer.keySet().size());
					Iterator<PlugInSelectableVOI> firstItr = voiBuffer.values().iterator();
					while(firstItr.hasNext()) {
						if((temp = firstItr.next()).getCalcEligible()) {
							calcItems.add(temp);
						}
					}
					ArrayList<PlugInSelectableVOI> orderedCalcItems = (ArrayList<PlugInSelectableVOI>)calcItems.clone();
					for(int j=0; j<calcItems.size(); j++) 
						orderedCalcItems.set(calcItems.get(j).getOutputLoc(), calcItems.get(j));
					dec = new DecimalFormat("0.##");
					for(int j=0; j<orderedCalcItems.size(); j++) {
						temp = orderedCalcItems.get(j);
						if(temp.getCreated()) {
							sliceStr[i] += dec.format(temp.getTotalArea(i))+"\t";
							sliceStr[i] += dec.format(temp.getFatArea(i))+"\t";
							sliceStr[i] += dec.format(temp.getLeanArea(i))+"\t";
							sliceStr[i] += dec.format(temp.getMeanFatH(i))+"\t";
							sliceStr[i] += dec.format(temp.getMeanLeanH(i))+"\t";
							sliceStr[i] += dec.format(temp.getMeanTotalH(i))+"\t";
						} else {
							sliceStr[i] += new String("0\t0\t0\t0\t0\t0\t");
						}
					}
				}
				
				return sliceStr;
			}
		}
	}
    /**
	 * Performs required calculations for plugin. Note partial voluming error likely exists for poor resolutions,
	 * should be quantified.
	 * 
	 * @author senseneyj
	 *
	 */
	private class MuscleCalculation extends Thread {
		
		//~ Static fields/initializers -------------------------------------------------------------------------------------
		
		public static final int OFFSET = 1024;
		public static final int FAT_LOWER_BOUND = -190;
		public static final int FAT_UPPER_BOUND = -30;
		public static final int MUSCLE_LOWER_BOUND = 0;
		public static final int MUSCLE_UPPER_BOUND = 100;
		public static final int FAR_LOWER_BOUND = -2048;
		public static final int FAR_UPPER_BOUND = 2048;
		
		//~ Instance fields -------------------------------------------------------------------------------------
		
		/**The current VOI being calculated.*/
		private PlugInSelectableVOI calculateVOI;
		
		/**Whether the Runnable has completed.*/
		private boolean done = false;
		
		/**The name of the current VOI being calculated.*/
		private String name;
		
		/**
		 * 
		 * @param newVOI the VOI to work on
		 * @param name the name of the VOI
		 */
		public MuscleCalculation(PlugInSelectableVOI newVOI, String name) {
			super();
			muscleCalcList.add(this);
			this.calculateVOI = newVOI;
			this.name = name;
		}
		
		/**
		 * Gets the VOI being calculated in this runnable.
		 */
		public VOI getCurrentVOI() {
			return calculateVOI;
		}

		/**
		 * Whether the runnable has finished.
		 */
		public boolean isFinished() {
			return done;
		}

		/**
		 * Performs all calculations for a particular VOI.
		 */
		public void run() {
			done = false;
			//ViewJProgressBar progressBar = new ViewJProgressBar("Calculations", "Initializing...", 0, 100, true);
			long time = System.currentTimeMillis();
			
			PlugInSelectableVOI[] children = new PlugInSelectableVOI[0];
			VOI v = calculateVOI;
			double wholeMultiplier = 0.0, sliceMultiplier = 0.0;
			
			//allows for calculation unit conversion from the source images base units, NIA requests that cm are normally used, but most scanners return mm resolutions
			int resultUnitLoc = FileInfoBase.getUnitsOfMeasureFromStr(((AnalysisDialogPrompt)tabs[resultTabLoc]).getSelectedOutput());
	        int res0Unit = getActiveImage().getUnitsOfMeasure(0);
	        int res1Unit = getActiveImage().getUnitsOfMeasure(1);
	        int res2Unit; //only used in 3D images
	        
	        float xRes = (float) (getActiveImage().getResolutions(0)[0] * ModelImage.getConversionFactor(resultUnitLoc, res0Unit));
	        float yRes = (float) (getActiveImage().getResolutions(0)[1] * ModelImage.getConversionFactor(resultUnitLoc, res1Unit));
	        float zRes; //only used in 3D images
			
			wholeMultiplier = sliceMultiplier = xRes*yRes;
			//for 3D images, also need to include z-resolution
			if(multipleSlices) {
				res2Unit = getActiveImage().getUnitsOfMeasure(2);
				zRes = (float) (getActiveImage().getResolutions(0)[2] * ModelImage.getConversionFactor(resultUnitLoc, res2Unit));
				wholeMultiplier *= zRes;
			}
			ViewUserInterface.getReference().getMessageFrame().append("Whole Multiplier: "+wholeMultiplier+"\tSliceMultiplier: "+sliceMultiplier+"\n", ViewJFrameMessage.DEBUG);
			PlugInSelectableVOI temp = voiBuffer.get(name);
			children = temp.getChildren();
			ArrayList<Thread> calc = new ArrayList<Thread>(children.length);
			Thread tempThread = null;
			
			for(int i=0; i<children.length; i++) {
				if(isInterrupted()) {
					muscleCalcList.remove(this);
					return;
				}
				if(children[i].getLastCalculated() == null || children[i].getLastModified() == null || 
						children[i].getLastCalculated().compareTo(children[i].getLastModified()) < 0) {
					//since enumerate silently ignores threads greater than array size
					//worst case scenario is extra calculation performed
					Thread[] activeGroup = new Thread[calcGroup.activeCount()];
					calcGroup.enumerate(activeGroup);
					//still check for nulls in case thread has been disposed
					boolean activeFound = false;
					for(int j=0; j<activeGroup.length; j++) {
						//ViewUserInterface.getReference().getMessageFrame().append("About to compare "+ activeGroup[j].getName() +" to "+residuals.get(i).getName());
						if(activeGroup[j] != null && activeGroup[j].getName().equals(children[i].getName())) {
							calc.add(activeGroup[j]);
							ViewUserInterface.getReference().getMessageFrame().append("Not calculating, because it's being calculated, should be yielding?: "+activeGroup[j]+"\n", ViewJFrameMessage.DEBUG);
							activeFound = true;
						}
					}
					if(!activeFound) {
						MuscleCalculation muscleCalc = new MuscleCalculation(children[i], children[i].getName());
			            //No thread group this time since just joining all later
						calc.add(tempThread = new Thread(muscleCalc));
			            tempThread.start();
					}
		            
				} else {
					ViewUserInterface.getReference().getMessageFrame().append("Just avoided calculating "+children[i].getName()+"\n", ViewJFrameMessage.DEBUG);
				}
			}
			long time2 = System.currentTimeMillis();
			ViewUserInterface.getReference().getMessageFrame().append("Waiting for threads to complete\n"+"\n", ViewJFrameMessage.DEBUG);
			if(isInterrupted()) {
				muscleCalcList.remove(this);
				return;
			}
			try {
				for(int i=0; i<calc.size(); i++) {
					calc.get(i).join();
				}
			} catch(InterruptedException e) {
				System.err.println("Algorithm failed, calculations may be inaccurate.");
				muscleCalcList.remove(this);
				e.printStackTrace();
				return;
			}
			ViewUserInterface.getReference().getMessageFrame().append("Time spent waiting: "+(System.currentTimeMillis() - time2)+" so that "+name+" can finish.\n", ViewJFrameMessage.DEBUG);
			
			//note that even for 3D images this will still be called area, even though refers to volume
			performCalculations(v, PlugInSelectableVOI.WHOLE_VOLUME_SLICE_NUMBER, wholeMultiplier);
			
			if(isInterrupted()) {
				muscleCalcList.remove(this);
				return;
			}
			

	        int[] iX = new int[]{0,0};
	        int[] iY = new int[]{0,0};
	        int[] iZ = new int[]{0,0};
	        temp.getBounds(iX,iY,iZ);
			ViewUserInterface.getReference().getMessageFrame().append("Number of slices: "+ (iZ[1] - iZ[0] + 1)+"\n", ViewJFrameMessage.DEBUG);
			//in the case of a 3D image, each VOI is cloned for a particular slice, relevant statistics are caluclated
			//for that slice only
			for(int k = iZ[0]; k <= iZ[1]; k++) {
				//progressBar.setMessage("Calculating "+name.toLowerCase()+" slice "+k+"...");
				VOI v2 = new VOI( (short)0, "temp" );
				Vector<VOIBase> sliceCurves = temp.getSliceCurves(k);
				for ( int s = 0; s < sliceCurves.size(); s++ )
				{
				    v2.importCurve( sliceCurves.elementAt(s) );
				}
				performCalculations(v2, k, sliceMultiplier);
			}
			time = System.currentTimeMillis() - time;
			calculateVOI.setCalculationTime(time);
			//only place where calculations are generated, setLastCalculated should only appear here
			temp.setLastCalculated(System.currentTimeMillis());
			
			if(isInterrupted()) {
				muscleCalcList.remove(this);
				return;
			}
			
			ViewUserInterface.getReference().getMessageFrame().append("Finished "+calculateVOI.getName()+" in "+time+"\n", ViewJFrameMessage.DEBUG);
			
			if(Preferences.getDebugLevels()[1]) {
				writeVoiLine();
			}
			
			done = true;
			muscleCalcList.remove(this);
		}

		/**
		 * Writes the statistics about this voi to a new line
		 */
		private void writeVoiLine() {
			PlugInSelectableVOI v = calculateVOI;
			File containerFolder = new File(Preferences.getPreferencesDir()+File.separator+"SegmentationStatistics"+File.separator);
			if(!containerFolder.exists())
				containerFolder.mkdir();
			File textFile = new File(Preferences.getPreferencesDir()+File.separator+"SegmentationStatistics"+File.separator+v.getName()+".txt");
			BufferedWriter writer = null;
			try {
				boolean doCreate = false;
				if(!textFile.exists()) {
					doCreate = true;
				}
				writer = new BufferedWriter(new FileWriter(textFile, !doCreate));
				if(doCreate) {
					String create = "File Location\tFat Area\tLean Area\tTotal Area\t"+
								"Mean Fat HU\tMean Lean HU\tMean Total HU\tDate\tSegmentation Time\tCalculation Time";
					writer.write(create);
					writer.newLine();
				}
				
				String s = new String(getActiveImage().getImageDirectory()+"\t"+v.getFatArea()+"\t"+v.getLeanArea()+"\t"+v.getTotalArea()+"\t"
										+v.getMeanFatH()+"\t"+v.getMeanLeanH()+"\t"+v.getMeanTotalH()+"\t"+
										new Date()+"\t"+v.getSegmentationTime()+"\t"+v.getCalculationTime());
				writer.write(s);
				writer.newLine();
				
				Preferences.debug("Wrote new line in file "+textFile+"\n");
			} catch(IOException e) {
				System.err.println("Error creating, writing or closing ucsd file.");
				e.printStackTrace();
				return;
			} finally {

	            try {
	                if (writer != null) {
	                    writer.flush();
	                    writer.close();
	                }
	            } catch (IOException ex) {
	                ex.printStackTrace();
	            }
			}
		}

		/**
		 * Perform calculation on voi slice given the slice number and individual multiplier
		 * @param v2
		 * @param sliceNumber
		 * @param multiplier
		 */
		private void performCalculations(VOI v2, int sliceNumber, double multiplier) {
			PlugInSelectableVOI temp = voiBuffer.get(name);
			PlugInSelectableVOI[] children = temp.getChildren();
			//note that even for 3D images this will still be called area, even though refers to volume
			double fatArea = getPieceCount(v2, FAT_LOWER_BOUND, FAT_UPPER_BOUND)*multiplier;
			double partialArea = getPieceCount(v2, FAT_UPPER_BOUND, MUSCLE_LOWER_BOUND)*multiplier; 
			double leanArea = getPieceCount(v2, MUSCLE_LOWER_BOUND, MUSCLE_UPPER_BOUND)*multiplier; 
			double totalAreaCount = getTotalAreaCount(v2)*multiplier;
			double fatAreaLarge = fatArea;
			double leanAreaLarge = leanArea; 
			double totalAreaLarge = totalAreaCount;
			//corrected area = abs(oldArea - sum(residual areas))
			for(int j=0; j<children.length; j++) {
				fatArea = Math.abs(fatArea - children[j].getFatArea(sliceNumber));
				partialArea = Math.abs(partialArea - children[j].getPartialArea(sliceNumber));
				leanArea = Math.abs(leanArea - children[j].getLeanArea(sliceNumber));
				totalAreaCount = Math.abs(totalAreaCount - children[j].getTotalArea(sliceNumber));
			}
			
			temp.setFatArea(fatArea, sliceNumber);
			temp.setLeanArea(leanArea, sliceNumber);
			temp.setPartialArea(partialArea, sliceNumber);
			temp.setTotalArea(totalAreaCount, sliceNumber);
			
			
			double meanFatH = getMeanH(v2, FAT_LOWER_BOUND, FAT_UPPER_BOUND);// + OFFSET;
			double meanLeanH = getMeanH(v2, MUSCLE_LOWER_BOUND, MUSCLE_UPPER_BOUND);// + OFFSET;
		    double meanTotalH = getMeanH(v2, FAR_LOWER_BOUND, FAR_UPPER_BOUND);// + OFFSET;
		    double meanFatHResidual = 0;
		    double meanLeanHResidual = 0;
		    double meanTotalHResidual = 0;
		    
		    //corrected mean = abs(oldMean*oldArea - sum(residualMean*residualArea))/abs(oldArea - sum(residualArea))
		    for(int j=0; j<children.length; j++) {
		    	meanFatHResidual += children[j].getMeanFatH(sliceNumber)*children[j].getFatArea(sliceNumber);
		    	meanLeanHResidual += children[j].getMeanLeanH(sliceNumber)*children[j].getLeanArea(sliceNumber);
		    	meanTotalHResidual += children[j].getMeanTotalH(sliceNumber)*children[j].getTotalArea(sliceNumber);
		    }
		    
		    meanFatH = (meanFatH*fatAreaLarge - meanFatHResidual) / fatArea;
		    meanLeanH = (meanLeanH*leanAreaLarge - meanLeanHResidual) / leanArea;
		    meanTotalH = (meanTotalH*totalAreaLarge - meanTotalHResidual) / totalAreaCount;
		    
		    if(meanFatH > 0) {
		    	meanFatH = -meanFatH;
		    	meanTotalH = -meanTotalH;
		    } else if(new Double(meanFatH).equals(Double.NaN)) 
		    	meanFatH = 0;
		    if(meanLeanH < 0) {
		    	meanLeanH = -meanLeanH;
		    } else if(new Double(meanLeanH).equals(Double.NaN)) {
		    	meanLeanH = 0;
		    }
		    if(new Double(meanTotalH).equals(Double.NaN)) {
		    	meanTotalH = 0;
		    }
		    
		    temp.setMeanFatH(meanFatH, sliceNumber);
			temp.setMeanLeanH(meanLeanH, sliceNumber);
			temp.setMeanTotalH(meanTotalH, sliceNumber);
		}
		
		/**
		 * Gets mean H-unit of all pixels that are bounded by lowerBound and upperBound.
		 */
		private double getMeanH(VOI v, int lowerBound, int upperBound) {
			int area = 0;
			double meanH = 0;
			BitSet fullMask = new BitSet();
			v.createBinaryMask3D(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1], false, false);
			double mark = 0;
			for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
		        mark = getImageA().getDouble(i);
				if(mark  >= lowerBound && mark <= upperBound) {
					area++;
					meanH += mark;
				}
			}
			meanH /= area;
			if(new Double(meanH).equals(Double.NaN)) {
				meanH = 0;
			}
			return meanH;
		}
		
		/**
		 * Gets the total number of pixels that are bounded by lowerBound and upperBound in the VOI.
		 * @return total number of pixels
		 */
		private double getPieceCount(VOI v, int lowerBound, int upperBound) {
			int area = 0;
			BitSet fullMask = new BitSet();
            v.createBinaryMask3D(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1], false, false);
			double mark = 0;
			for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
		        mark = getImageA().getDouble(i);
				if(mark  >= lowerBound && mark <= upperBound) { 
					area++;	
				}
			}
			return area;
		}

		/**
		 * Gets new calculated number of pixels that are in the area of the VOI.
		 */
		private double getTotalAreaCount(VOI v) {
			int totalArea = 0;
			BitSet fullMask = new BitSet();
            v.createBinaryMask3D(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1], false, false);
			for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
		        totalArea++;
			}
			return totalArea;
		}
	}
	/**
     * Builds thigh axis through two thigh image.
     * 
     * @author senseneyj
     *
     */
	private class BuildThighAxes implements AlgorithmInterface {
		
	    private int zSlice;
	    
	    private ModelImage image;
	    
	    private boolean axesCompleted;
	    
	    private int[] defaultPts;
	    
	    private VOIVector VOIs;
	    
	    private int groupNum;
	    
	    private int thighIndex;
	    
	    private AlgorithmBSmooth[] smoothAlgo;
	    
	    private PlugInSelectableVOI[] thighVOIs;
	    
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
	            ViewUserInterface.getReference().getMessageFrame().append("B Smooth completed\n", ViewJFrameMessage.DEBUG);
	            if (smoothAlgo[thighIndex].isCompleted() == true && thighCompleted[thighIndex]) {
	
	                // The algorithm has completed and produced a
	                resultVOI = smoothAlgo[thighIndex].getResultVOI();
	                image.registerVOI(resultVOI);
	                //build axes here
	                axesCompleted = true;
	            }
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

		public boolean getAxesCompleted() {
	        return axesCompleted;
	    }
	
	    private void initThighAxes() {
	        defaultPts = new int[2];
	        int nVOI;//, nContours;
	        float[] xPoints = null;
	        float[] yPoints = null;
	    
	        VOIs = image.getVOIs(); //note that VOIs must already be loaded
	    
	        nVOI = VOIs.size();
	    
	        if (nVOI == 0) {
	            return;
	        }
	        
	        thighVOIs = new PlugInSelectableVOI[2];
	        
	        for (groupNum = 0; groupNum < nVOI; groupNum++) {
	    
	            if (VOIs.get(groupNum).getName().equals("Left Thigh")) {
	                thighVOIs[0] = null;//VOIs.get(groupNum);
	            }
	            else if (VOIs.get(groupNum).getName().equals("Right Thigh")) {
	                thighVOIs[1] = null;//VOIs.get(groupNum);
	            }
	        }
	        
	        //No thighs found
	        if (groupNum == nVOI) {
	            MipavUtil.displayError("No whole thighs were found.  Cannot compute axes.  "+
	                                    "Please ensure that whole thighs are defined as seperate VOIs for this image.");
	            return;
	        }
	        
	        for(int i=0; i<thighVOIs.length; i++) {            	    
	            int elementNum = 0;
	       
	            Vector<VOIBase> contours = thighVOIs[i].getSliceCurves(zSlice);
	            VOIBase selectedContour = contours.elementAt(elementNum);
	            int npoints = selectedContour.size();
	            
	            xPoints = new float[npoints + 5];
	            yPoints = new float[npoints + 5];
	
	            xPoints[0] = selectedContour.elementAt(npoints - 2).X;
	            yPoints[0] = selectedContour.elementAt(npoints - 2).Y;
	
	            	xPoints[1] = selectedContour.elementAt(npoints - 1).X;
	            	yPoints[1] = selectedContour.elementAt(npoints - 1).Y;
	            
	            for (i = 0; i < npoints; i++) {
	                xPoints[i + 2] = selectedContour.elementAt(i).X;
	                yPoints[52*i + 2] = selectedContour.elementAt(i).Y;
	            }
	
	            xPoints[npoints + 2] = selectedContour.elementAt(0).X;
	            yPoints[npoints + 2] = selectedContour.elementAt(0).Y;
	
	            xPoints[npoints + 3] = selectedContour.elementAt(1).X;
	            yPoints[npoints + 3] = selectedContour.elementAt(1).Y;
	
	            xPoints[npoints + 4] = selectedContour.elementAt(2).X;
	            yPoints[npoints + 4] = selectedContour.elementAt(2).Y;
	
	            AlgorithmArcLength arcLength = new AlgorithmArcLength(xPoints, yPoints);
	            defaultPts[i] = Math.round(arcLength.getTotalArcLength() / 6); //larger denom.
	        }
	    }
	}
		
	
	
	/**
	 * Custom livewire for adhering to edges of already created VOIs.
	 * 
	 * @author senseneyj
	 *
	 */
	private class CustomLivewire extends RubberbandLivewire {
		
		/**A set representing all VOIs that should be avoided */
		private BitSet voiMap; 
		
		/**The length of this image in the x direction, normally 512.*/
		private int xRes;
		
		/**
	     * Sets up local costs graph by calling AlgorithmGradientMagnitude and AlgorithmEdgeLaplacianSep. Initializes
	     * necessary global arrays.
	     *
	     * @param  component  component to add to
	     * @param  selection  GRADIENT_MAG, MEDIALNESS, or INTENSITY
	     */
	    public CustomLivewire(Component component, int selection) {
	    	super(component, selection);
	    
	    	xRes = getActiveImage().getExtents(0)[0];    	
	    }
		
	    /**
	     * Stretch the rubberband to this point.
	     *
	     * @param  p  point to stretch to
	     */
	    public void stretch(Point p) {
	    	//if(voiSearch(p)) {
	    	//	lastPt.x = stretchedPt.x;
	        //    lastPt.y = stretchedPt.y;
	        //}
	        super.stretch(p);
	    }
	    
	    /**
	     * Determines whether a VOI that should be avoided exists at the current point.
	     * @param pt The current point
	     * @return true if a VOI that should be avoided exists at current point
	     */
	    private boolean voiSearch(Point pt) {
	    	return voiMap.get(pt.x+pt.y*xRes);
	    }
	}
	
	private class PlugInMuscleEditImage extends ViewJComponentEditImage {
		
		public PlugInMuscleEditImage(PlugInMuscleImageDisplay _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
                ModelImage _imageB, ModelLUT _LUTb, float[] imgBufferB, int[] pixelBuffer,
                float zoom, int[] extents, boolean logMagDisplay, int _orientation) {
			super(_frame, _imageA, _LUTa, imgBufferA, _imageB, _LUTb, imgBufferB, pixelBuffer, zoom, extents, logMagDisplay, _orientation);
			
			//voiHandler = new PlugInHandler(this);
			
			//addMouseListener(voiHandler);
            //addMouseMotionListener(voiHandler);
		}
		
		/**
	     * Opens a JDialogStatistics to allow computation ofROI statistics.
	     */
	    public void showStatisticsCalculator() {

	        if (imageStatList == null) {

	            if ((imageActive.getVOIs() != null) && (imageActive.getVOIs().size() != 0)) {
	                String[] activeCalc = ((PlugInMuscleImageDisplay)frame).getCalcRunning();
	                if(activeCalc == null) {
	                	VOIVector createdVOIs = new VOIVector();
	                	for(int i=0; i<imageActive.getVOIs().size(); i++) 
	                		if(!imageActive.getVOIs().get(i).isEmpty())
	                			createdVOIs.add(imageActive.getVOIs().get(i));
	                	imageStatList = new PlugInVOIStatistics(createdVOIs);
	                	imageStatList.setVisible(true);
	                	// addVOIUpdateListener(imageStatList);
	                } else {
	                	String active = new String();
	                	for(int i=0; i<activeCalc.length; i++) {
	                		active += activeCalc[i]+"\n";
	                	}
	                	MipavUtil.displayError("Please wait for the following calculations to complete:\n"+active);
	                }
	            } else {
	                MipavUtil.displayError("A VOI must be present to use the statistics calculator");
	            }
	        } else {
	            imageStatList.refreshVOIList(imageActive.getVOIs());
	            imageStatList.setVisible(true);
	        }
	    }
	}
	
	private class PlugInVOIStatistics extends JDialogVOIStatistics {
		 /**
	     * builds and packs the frame. does <i>not</I> set it visible.
	     *
	     * <p>install the panels of source directory, destination directory, the checkbox for approving the
	     * translation-table file and the panel containing the ok and cancel buttons. Installs the checkbox panel.</p>
	     *
	     * @param  voiList  list of existing vois (possibly without area/volume) that exist in this image.
	     */
	    public PlugInVOIStatistics(VOIVector voiList) {
	        super(voiList);

	        //checkBoxPanel = new PlugInStatisticsList();
	    }
	    
	    protected void buildDialog(VOIVector voiList) {
	    	setTitle("Calculate Statistics on VOI groups");
	        setJMenuBar(buildMenuEntries());
	        buildToolBar();
	        this.userInterface = ViewUserInterface.getReference();
	        image = ViewUserInterface.getReference().getActiveImageFrame().getComponentImage().getActiveImage();
	        xUnits = image.getFileInfo(0).getUnitsOfMeasure()[0];
	        yUnits = image.getFileInfo(0).getUnitsOfMeasure()[1];
	        zUnits = FileInfoBase.UNKNOWN_MEASURE;

	        if (image.getNDims() > 2) {
	            zUnits = image.getFileInfo(0).getUnitsOfMeasure()[2];
	        }
	        // need to take out line VOIs, polyline VOIs, point VOIs

	        everything = new JTabbedPane(SwingConstants.TOP);
	        everything.setFont(MipavUtil.font12B);
	        everything.insertTab("VOI selection", null, buildVOIPanel(voiList), // we must store this panel so we can
	                                                                            // create a new listing later
	                             "Choose VOIs and statistics file", VOI_TAB);

	        JPanel statPanel = new JPanel(new BorderLayout());
	        checkBoxPanel = new PlugInStatisticsList();
	        
	        try {
	            checkBoxPanel.setSliceCount(image.getExtents()[2]);
	        } catch (ArrayIndexOutOfBoundsException aioobe) {

	            // otherwise, this must be a 2d image.
	            checkBoxPanel.setSliceCount(1);
	        } finally {
	            checkBoxPanel.setCheckBoxesEnabled();
	        }
	        
	        outputOptionsPanel = new JPanelStatisticsOptions();

	        if (ViewUserInterface.getReference().getActiveImageFrame().getComponentImage().getActiveImage().getNDims() ==
	                2) {
	            outputOptionsPanel.setBySliceEnabled(false);
	        }

	        statPanel.add(outputOptionsPanel, BorderLayout.EAST);
	        statPanel.add(checkBoxPanel, BorderLayout.CENTER);
	        everything.insertTab("Statistics Options", null, statPanel, "Statistic Selection", STAT_TAB);

	        everything.insertTab("Logging", null, buildLogPanel(), "Output Log", LOG_TAB);

	        getContentPane().add(toolBar, BorderLayout.NORTH);
	        getContentPane().add(everything, BorderLayout.CENTER);
	        getContentPane().add(buildOKCancelPanel(), BorderLayout.SOUTH); // build OK/Cancel button Panel

	        pack();
	        setSize(800, 500); // decent size??
	    }
	    
	    /**
		 * creates a new keylog, writing which tags are to be removed from the image information; the table header for the
		 * image read/write logging is added. the string created here is not automatically turned into the keylog string.
		 * that must be done by the caller.
		 *
		 * @return  the new KeyLog String.
		 */
		protected String createNewLogfile() {
		    int i;
		    FileInfoDicom fileInfo = (FileInfoDicom)getActiveImage().getFileInfo()[0];
		    String id = (String)fileInfo.getTagTable().getValue("0010,0020");
			id = id != null && id.length() > 0 ? id.trim() : "Removed";
			
			String dob = (String)fileInfo.getTagTable().getValue("0010,0030");
			dob = dob != null && dob.length() > 0 ? dob.trim() : "Unknown";
			
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
			Date date = new Date();
			String analysisDate = dateFormat.format(date);
		
			String scanDate = (String)fileInfo.getTagTable().getValue("0008,0020");
			scanDate = scanDate != null ? scanDate.trim() : "Unknown";
		
			String sliceNumber = new String();
			sliceNumber = (String)fileInfo.getTagTable().getValue("0020,0013");
			sliceNumber = sliceNumber != null && sliceNumber.length() > 0 ? sliceNumber.trim() : "Unknown";
				
			String userName = System.getProperty("user.name");
			userName = userName != null ? userName.trim() : "Unknown";
			
		    String kl = "#\tPatient ID:\t"+id+"\tPatient DOB:\t"+dob+"\tScan date:\t"+scanDate+"\n";
		    kl += "#"+"\tSlice:\t"+ sliceNumber+"\tAnalyst:\t"+userName+"\tAnalysis Date:\t"+analysisDate+"\n";
		    String str;
		
		    // output the labels of the list of statistics to be produced.
		    String[] checklistLabels = new PlugInStatisticsList().makeCheckboxLabels();
		    kl += "Name, Slice, Contour\t";
		
		    for (i = 0; i < checklistLabels.length; i++) {
		
		        if (checkList[i]) {
		
		            if ((checklistLabels[i].equals("Volume")) && (xUnits == yUnits) && (xUnits == zUnits) &&
		                    (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
		                str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();
		                kl += checklistLabels[i] + " (" + str + ")" + "\t";
		            } else if ((checklistLabels[i].equals("Area")) && (xUnits == yUnits) &&
		                           (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
		                str = image.getFileInfo(0).getAreaUnitsOfMeasureStr();
		                kl += checklistLabels[i] + " (" + str + ")" + "\t";
		            } else if ((checklistLabels[i].equals("Perimeter")) && (xUnits == yUnits) &&
		                           (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
		                str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
		                kl += checklistLabels[i] + " (" + str + ")" + "\t";
		            } else if (checklistLabels[i].equals("Principal Axis")) {
		                kl += checklistLabels[i] + " (degrees)" + "\t";
		            } else if ((checklistLabels[i].equals("Major axis length")) && (xUnits == yUnits) &&
		                           (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
		                str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
		                kl += checklistLabels[i] + " (" + str + ")" + "\t";
		            } else if ((checklistLabels[i].equals("Minor axis length")) && (xUnits == yUnits) &&
		                           (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
		                str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
		                kl += checklistLabels[i] + " (" + str + ")" + "\t";
		            } else {
		                kl += checklistLabels[i] + "\t";
		            }
		        }
		    }
		
		    kl += "\n";
		
		    return kl;
		}

		/**
	     * Method for updating the table and GUI after the algorithm has completed (Not for script-running).
	     */
	    protected void updateDialog() {

	        // notification will turn buttons back on
	        cancelButton.setEnabled(true);
	        OKButton.setEnabled(true);

	        // get output data out of the notifier
	        // getStatisticsData((AlgorithmVOIProps)event);

	        if (!calculator.isCompleted()) {
	            return;
	        }

	        int totalCount = 0;
	        String str;
	        VOIStatisticalProperties properties;
	        Vector[] contours;

	        if ((processType == AlgorithmVOIProps.PROCESS_PER_SLICE) ||
	                (processType == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR)) {
	            ListModel list = selectedList.getModel();

	            if (logModel.getColumnIndex("Name, Slice, Contour") == -1) {
	                logModel.addColumn("Name, Slice, Contour");
	            }

	            for (int i = 0; i < VOIStatisticList.numberOfStatistics; i++) {

	                if (checkList[i]) {

	                    if (logModel.getColumnStartsWithIndex(VOIStatisticList.statisticDescription[i]) == -1) {

	                        if ((VOIStatisticList.statisticDescription[i].indexOf("Volume") != -1) && (xUnits == yUnits) &&
	                                (xUnits == zUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
	                            str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
	                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Area") != -1) &&
	                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
	                            str = image.getFileInfo(0).getAreaUnitsOfMeasureStr();
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
	                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Perimeter") != -1) &&
	                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
	                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
	                        } else if (VOIStatisticList.statisticDescription[i].indexOf("Principal Axis") != -1) {
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (degrees)");
	                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Major axis length") != -1) &&
	                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
	                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
	                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Minor axis length") != -1) &&
	                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
	                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
	                        } else {
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i]);
	                        }
	                    }

	                    // total count used for total # of data elemets, need to add 3 if color
	                    // image and intensity related (R,G,B)
	                    totalCount++;

	                    if (calculator.isColor() && (VOIStatisticList.statisticDescription[i].indexOf("Intensity") != -1)) {
	                        totalCount += 2;
	                    }
	                }
	            }
	            
	            String[] allNames = new PlugInStatisticsList().makeCheckboxLabels();
	            
	            for(int i=VOIStatisticList.statisticDescription.length; i<checkBoxPanel.getCheckboxLength(); i++) {
	            	String name = allNames[i];
	            	if (checkBoxPanel.getSelectedList(name)) {

	                    if (logModel.getColumnIndex(name) == -1) {
                            logModel.addColumn(name);
	                    }
	            	}
	            }

	            // for each element in the list ....
	            for (int i = 0; i < list.getSize(); i++) {
	                properties = calculator.getVOIProperties((VOI) list.getElementAt(i));
	                int zDim = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
	                contours = ((VOI) list.getElementAt(i)).getSortedCurves(zDim);

	                String[] rowData = new String[logModel.getColumnCount()];
	                String[] totalData = new String[logModel.getColumnCount()];

	                String[] logRowData = new String[rowData.length];
	                String[] logTotalData = new String[rowData.length];

	                for (int slice = 0; slice < contours.length; slice++) {
	                    int count = 0;
	                    int stop = 1;
	                    String end = slice + ";";

	                    if (calculator.getProcessType() == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
	                        stop = contours[slice].size();
	                    }

	                    if (contours[slice].size() < 1) {
	                        stop = 0;
	                    }

	                    // for each contour only print titles and calculations once,
	                    // if not "calculate by contour" (ie., if we only want totals):
	                    for (int num = 0; num < stop; num++) {

	                        // first: set up row title:
	                        rowData[0] = list.getElementAt(i).toString() + ", " + // VOI name
	                                     (slice + 1) + ", " + // slice #, irrellevent to where contour is in image
	                                     ((VOIBase) contours[slice].get(num)).getLabel(); // contour #, held in label
	                        totalData[0] = "Totals:";

	                        logRowData[0] = new String(rowData[0]);
	                        logTotalData[0] = new String(totalData[0]);

	                        if (calculator.getProcessType() == AlgorithmVOIProps.PROCESS_PER_SLICE_AND_CONTOUR) {
	                            end = slice + ";" + num;
	                        }

	                        // for each column in the row, print the statistic:
	                        for (int k = 0; k < statisticDescription.length; k++) {

	                            if (logModel.getColumnBaseIndex(VOIStatisticList.statisticDescription[k]) != -1) {
	                                count++;
	                            }

	                            if (checkList[k]) {

	                                // if it's a color image and the property is min intensity, max intensity, avg
	                                // intensity, or standard deviation of intensity, those properties were entered as Red,
	                                // Green, Blue and we should display them differently.
	                                if (calculator.isColor() &&
	                                        (VOIStatisticList.statisticDescription[k].indexOf("Intensity") != -1)) {
	                                    String temp = "R: " + properties.getProperty(statisticDescription[k] + "Red" + end);
	                                    temp += " G: " + properties.getProperty(statisticDescription[k] + "Green" + end);
	                                    temp += " B: " + properties.getProperty(statisticDescription[k] + "Blue" + end);
	                                    rowData[count] = temp;
	                                    logRowData[count] = temp;

	                                    if (showTotals) {
	                                        temp = " R: " + properties.getProperty(statisticDescription[k] + "RedTotal");
	                                        temp += " G: " + properties.getProperty(statisticDescription[k] + "GreenTotal");
	                                        temp += " B: " + properties.getProperty(statisticDescription[k] + "BlueTotal");
	                                        totalData[count] = temp;
	                                        logTotalData[count] = temp;
	                                    }
	                                } else {

	                                    rowData[count] = properties.getProperty(statisticDescription[k] + end).replaceAll("\t",
	                                                                                                                      ", ");
	                                    logRowData[count] = properties.getProperty(statisticDescription[k] + end);

	                                    if (showTotals) {
	                                        totalData[count] = properties.getProperty(statisticDescription[k] + "Total").replaceAll("\t",
	                                                                                                                                ", ");
	                                        logTotalData[count] = properties.getProperty(statisticDescription[k] + "Total");
	                                    }
	                                }
	                            }
	                        } // end for each column
	                        
	                        for(int k=statisticDescription.length; k<allNames.length; k++) {
	    	                	if (logModel.getColumnBaseIndex(allNames[k]) != -1) {
	    	                        count++;
	    	                    }

	    	                    if (checkList[k]) {
	    	                    	//Guaranteed to not be color image
	    	                    	DecimalFormat dec = new DecimalFormat("0.00");
	    	                    	VOI v = (VOI) list.getElementAt(i);
	    	                    	boolean doAdd = false;
	    	                    	double addAmount = 0.0;
	    	                    	if(v instanceof PlugInSelectableVOI && ((PlugInSelectableVOI)v).getCalcEligible()) {
	    	                    		if(allNames[k].equals(TOTAL_AREA)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getTotalArea(slice));
	                        				logRowData[count] = dec.format(((PlugInSelectableVOI)v).getTotalArea(slice));
	                        				addAmount = ((PlugInSelectableVOI)v).getTotalArea(slice);
	                        				doAdd = true;
	    	                    		} else if(allNames[k].equals(FAT_AREA)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getFatArea(slice));
	                        				logRowData[count] = dec.format(((PlugInSelectableVOI)v).getFatArea(slice));
	                        				addAmount = ((PlugInSelectableVOI)v).getFatArea(slice);
	                        				doAdd = true;
	    	                    		} else if(allNames[k].equals(LEAN_AREA)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getLeanArea(slice));
	                        				logRowData[count] = dec.format(((PlugInSelectableVOI)v).getLeanArea(slice));
	                        				addAmount = ((PlugInSelectableVOI)v).getLeanArea(slice);
	                        				doAdd = true;
	    	                    		} else if(allNames[k].equals(MEAN_TOTAL_HU)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanTotalH(slice));
	                        				logRowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanTotalH(slice));
	                        				addAmount = ((PlugInSelectableVOI)v).getMeanTotalH(slice);
	                        				doAdd = true;
	    	                    		} else if(allNames[k].equals(MEAN_FAT_HU)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanFatH(slice));
	                        				logRowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanFatH(slice));
	                        				addAmount = ((PlugInSelectableVOI)v).getMeanFatH(slice);
	                        				doAdd = true;
	    	                    		} else if(allNames[k].equals(MEAN_LEAN_HU)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanLeanH(slice));
	                        				logRowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanLeanH(slice));
	                        				addAmount = ((PlugInSelectableVOI)v).getMeanLeanH(slice);
	                        				doAdd = true;
	    	                    		}
	    	                    		if(doAdd) {
	    	                    			if(logTotalData[count] != null) {
	    	                    				logTotalData[count] = dec.format(Double.valueOf(logTotalData[count])+addAmount);
	    	                    			} else {
	    	                    				logTotalData[count] = dec.format(addAmount);
	    	                    			}
	    	                    		} 
	    	                    	}
	    	                    }
	    	                }

	                        count = 0;
	                        logModel.addRow(rowData);

	                        String logText = "";

	                        for (int j = 0; j < rowData.length; j++) {
	                            logText += logRowData[j] + "\t";
	                        }

	                        writeLogfileEntry(logText);
	                    } // end for contours
	                }

	                if (showTotals) {
	                    logModel.addRow(totalData);

	                    String logText = "";

	                    for (int j = 0; j < logTotalData.length; j++) {
	                        //logTotalData[j] are simply summed area elements, these are not anatomically interesting
	                    	logText += logTotalData[j] + "\t";
	                    }

	                    writeLogfileEntry(logText);
	                }

	                for (int k = 0; k < rowData.length; k++) {
	                    rowData[k] = "";
	                }

	                logModel.addRow(rowData);
	            }
	        } else { // whole 3D VOI data

	            ListModel list = selectedList.getModel();

	            if (logModel.getColumnIndex("Name, Slice, Contour") == -1) {
	                logModel.addColumn("Name, Slice, Contour");
	            }

	            // add any columns which will be displayed, but not already displayed:
	            for (int i = 0; i < VOIStatisticList.numberOfStatistics; i++) {

	                if (checkBoxPanel.getSelectedList(VOIStatisticList.statisticDescription[i])) {

	                    if (logModel.getColumnIndex(VOIStatisticList.statisticDescription[i]) == -1) {

	                        if ((VOIStatisticList.statisticDescription[i].indexOf("Volume") != -1) && (xUnits == yUnits) &&
	                                (xUnits == zUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
	                            str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
	                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Area") != -1) &&
	                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
	                            str = image.getFileInfo(0).getAreaUnitsOfMeasureStr();
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
	                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Perimeter") != -1) &&
	                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
	                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
	                        } else if (VOIStatisticList.statisticDescription[i].indexOf("Principal Axis") != -1) {
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (degrees)");
	                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Major axis length") != -1) &&
	                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
	                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
	                        } else if ((VOIStatisticList.statisticDescription[i].indexOf("Minor axis length") != -1) &&
	                                       (xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
	                            str = FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
	                        } else {
	                            logModel.addColumn(VOIStatisticList.statisticDescription[i]);
	                        }
	                    }

	                    // total count used for total # of data elemets, need to add 3 if color image and intensity related
	                    // (R,G,B)
	                    totalCount++;

	                    if (calculator.isColor() && (VOIStatisticList.statisticDescription[i].indexOf("Intensity") != -1)) {
	                        totalCount += 2;
	                    }
	                }
	            }
	            
	            String[] allNames = new PlugInStatisticsList().makeCheckboxLabels();
	            
	            for(int i=VOIStatisticList.statisticDescription.length; i<checkBoxPanel.getCheckboxLength(); i++) {
	            	String name = allNames[i];
	            	if (checkBoxPanel.getSelectedList(name)) {

	                    if (logModel.getColumnIndex(name) == -1) {
                            logModel.addColumn(name);
	                    }
	            	}
	            }

	            DecimalFormat dec = new DecimalFormat("0.00");
	            // for each element in the list print properties of each VOI,
	            // column-by-column:
	            for (int i = 0; i < list.getSize(); i++) {
	                properties = calculator.getVOIProperties((VOI) list.getElementAt(i));

	                String[] rowData = new String[logModel.getColumnCount()];
	                rowData[0] = list.getElementAt(i).toString();

	                int count = 0;

	                for (int k = 0; k < statisticDescription.length; k++) {

	                    if (logModel.getColumnBaseIndex(VOIStatisticList.statisticDescription[k]) != -1) {
	                        count++;
	                    }

	                    if (checkList[k]) {

	                        // if it's a color image and the property is min intensity, max intensity, avg intensity,
	                        // or standard deviation of intensity, those properties were entered as Red, Green, Blue and
	                        // we should display them differently.
	                        if (calculator.isColor() &&
	                                (VOIStatisticList.statisticDescription[k].indexOf("Intensity") != -1)) {
	                            String temp = "R: " + properties.getProperty(statisticDescription[k] + "Red");
	                            temp += " G: " + properties.getProperty(statisticDescription[k] + "Green");
	                            temp += " B: " + properties.getProperty(statisticDescription[k] + "Blue");
	                            rowData[count] = temp;
	                        } else {
	                            rowData[count] = properties.getProperty(statisticDescription[k]);
	                        }
	                    }
	                } // end for each column
	                
	                for(int k=statisticDescription.length; k<allNames.length; k++) {
	                	if (logModel.getColumnBaseIndex(allNames[k]) != -1) {
	                        count++;
	                    }

	                    if (checkList[k]) {
	                    	//Guaranteed to not be color image
	                    	VOI v = (VOI) list.getElementAt(i);
	                    	if(v instanceof PlugInSelectableVOI && ((PlugInSelectableVOI)v).getCalcEligible()) {
	                    		if(allNames[k].equals(TOTAL_AREA)) {
                    				rowData[count] = dec.format(((PlugInSelectableVOI)v).getTotalArea());
	                    		} else if(allNames[k].equals(FAT_AREA)) {
                    				rowData[count] = dec.format(((PlugInSelectableVOI)v).getFatArea());
	                    		} else if(allNames[k].equals(LEAN_AREA)) {
                    				rowData[count] = dec.format(((PlugInSelectableVOI)v).getLeanArea());
	                    		} else if(allNames[k].equals(MEAN_TOTAL_HU)) {
                    				rowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanTotalH());
	                    		} else if(allNames[k].equals(MEAN_FAT_HU)) {
                    				rowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanFatH());
	                    		} else if(allNames[k].equals(MEAN_LEAN_HU)) {
                    				rowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanLeanH());
	                    		}
	                    	}
	                    }
	                	
	                }

	                count = 0;
	                logModel.addRow(rowData);

	                String logText = "";

	                for (int j = 0; j < rowData.length; j++) {
	                    logText += rowData[j] + "\t";
	                }

	                writeLogfileEntry(logText);

	                for (int k = 0; k < rowData.length; k++) {
	                    rowData[k] = "";
	                }

	                logModel.addRow(rowData);
	            }
	        }

	        // finalise the output details
	    }
	}
	
	private class PlugInStatisticsList extends JPanelStatisticsList {
		
		private String[] calcExtra;
		
		/**
	     * Creates the list of labels to use in the checkboxes.
	     *
	     * @return  DOCUMENT ME!
	     */
	    public String[] makeCheckboxLabels() {
	    	calcExtra = new String[6];
			
			calcExtra[0] = "Total Area";
			calcExtra[1] = "Fat Area";
			calcExtra[2] = "Lean Area";
			calcExtra[3] = "Mean Total HU";
			calcExtra[4] = "Mean Fat HU";
			calcExtra[5] = "Mean Lean HU";
	    	
	    	String[] labels = new String[statisticDescription.length+calcExtra.length];
	    	
	    	for(int i=0; i<statisticDescription.length; i++) {
	    		ViewUserInterface.getReference().getMessageFrame().append("Orginial fill: "+i+"\n", ViewJFrameMessage.DEBUG);
	        	labels[i] = statisticDescription[i];
	        }
	    	
	    	for(int i=statisticDescription.length; i<labels.length; i++) {
	    		ViewUserInterface.getReference().getMessageFrame().append("Next step: "+i+"\n", ViewJFrameMessage.DEBUG);
	    		labels[i] = calcExtra[i-statisticDescription.length];
	    	}
	    	
	    	return labels;
	    }
	    
	    protected void setListLength() {
	        listLength = numberOfStatistics+calcExtra.length;
	    }
			
	}

	 //TODO: Work with right mouse click
	
//	private class PlugInVOIStats extends JDialogVOIStats {
//		
//	    public PlugInVOIStats(Frame theParentFrame, ModelImage img, VOI _voi) {
//	        super(theParentFrame, img, _voi);
//	    }
//
//	    protected void init() {
//
//	        // setTitle("VOI Statistics");
//	        frameBorder = BorderFactory.createCompoundBorder(BorderFactory.createRaisedBevelBorder(),
//	                                                         BorderFactory.createLoweredBevelBorder());
//
//	        JLabel labelName = new JLabel("Name of VOI:");
//	        labelName.setFont(serif12);
//	        labelName.setForeground(Color.black);
//
//	        JLabel labelColor = new JLabel("Color of VOI:");
//	        labelColor.setFont(serif12);
//	        labelColor.setForeground(Color.black);
//
//	        JLabel labelThickness = new JLabel("Thickness of VOI:");
//	        labelThickness.setFont(serif12);
//	        labelThickness.setForeground(Color.black);
//
//	        colorButton = new JButton();
//	        colorButton.setPreferredSize(new Dimension(25, 25));
//	        colorButton.setToolTipText("Change VOI color");
//	        colorButton.addActionListener(this);
//
//	        VOIName = new JTextField(15);
//	        VOIName.setFont(serif12);
//
//	        VOIThicknessField = new JTextField(3);
//	        VOIThicknessField.setFont(serif12);
//	        MipavUtil.makeNumericsOnly(VOIThicknessField, false);
//
//	        JPanel namePanel = new JPanel(new GridBagLayout());
//	        GridBagConstraints gbc = new GridBagConstraints();
//	        gbc.insets = new Insets(5, 5, 5, 5);
//	        gbc.anchor = GridBagConstraints.WEST;
//	        gbc.weightx = 0;
//	        gbc.weighty = 0;
//	        gbc.fill = GridBagConstraints.NONE;
//	        namePanel.add(labelName, gbc);
//
//	        gbc.weightx = 1;
//	        gbc.weighty = 1;
//	        gbc.gridx = 1;
//	        gbc.fill = GridBagConstraints.HORIZONTAL;
//	        namePanel.add(VOIName, gbc);
//
//	        gbc.weightx = 0;
//	        gbc.weighty = 0;
//	        gbc.gridx = 0;
//	        gbc.gridy = 1;
//	        gbc.fill = GridBagConstraints.NONE;
//	        namePanel.add(labelThickness, gbc);
//
//	        gbc.weightx = 1;
//	        gbc.weighty = 1;
//	        gbc.gridx = 1;
//	        gbc.fill = GridBagConstraints.HORIZONTAL;
//	        namePanel.add(VOIThicknessField, gbc);
//
//	        gbc.gridx = 0;
//	        gbc.gridy = 2;
//	        gbc.weightx = 0;
//	        gbc.weighty = 0;
//	        gbc.fill = GridBagConstraints.NONE;
//	        namePanel.add(labelColor, gbc);
//
//	        gbc.gridx = 1;
//	        gbc.weightx = 1;
//	        gbc.weighty = 1;
//	        namePanel.add(colorButton, gbc);
//
//	        gbc.insets = new Insets(0, 0, 0, 0);
//
//	        checkboxBoundingBox = new JCheckBox("Show contour bounding box");
//	        checkboxBoundingBox.setFont(serif12);
//
//	        checkboxAdditiveOrSubtractive = new JCheckBox("Use additive polarity for VOI");
//	        checkboxAdditiveOrSubtractive.setFont(serif12);
//
//	        checkboxIncludeForProcessing = new JCheckBox("Include for processing");
//	        checkboxIncludeForProcessing.setFont(serif12);
//
//	        checkboxBoundary = new JCheckBox("Display VOI shading");
//	        checkboxBoundary.setFont(serif12);
//	        checkboxBoundary.addItemListener(this);
//
//	        checkboxVOIName = new JCheckBox("Show VOI name");
//	        checkboxVOIName.setFont(serif12);
//	        checkboxVOIName.setSelected(Preferences.is(Preferences.PREF_SHOW_VOI_NAME));
//
//	        JPanel checkboxPanel = new JPanel();
//	        checkboxPanel.setLayout(new BoxLayout(checkboxPanel, BoxLayout.Y_AXIS));
//	        checkboxPanel.add(checkboxBoundingBox);
//	        checkboxPanel.add(checkboxAdditiveOrSubtractive);
//	        checkboxPanel.add(checkboxIncludeForProcessing);
//	        checkboxPanel.add(checkboxVOIName);
//	        checkboxPanel.add(checkboxBoundary);
//
//	        opacitySlider = new JSlider(SwingConstants.HORIZONTAL, 0, 100, 30);
//
//	        opacitySlider.setMajorTickSpacing(20);
//	        opacitySlider.setValue(30);
//	        opacitySlider.setPaintTicks(true);
//	        opacitySlider.setEnabled(false);
//	        opacitySlider.addChangeListener(this);
//
//	        JLabel maximum = new JLabel(String.valueOf(1));
//	        maximum.setForeground(Color.black);
//	        maximum.setFont(serif12);
//
//	        current = new JLabel(String.valueOf(opacitySlider.getValue() / 100.0f));
//	        current.setForeground(Color.black);
//	        current.setFont(serif12B);
//
//	        JLabel minimum = new JLabel(String.valueOf(0));
//	        minimum.setForeground(Color.black);
//	        minimum.setFont(serif12);
//
//	        JPanel sliderPanel = new JPanel(new GridBagLayout());
//
//	        gbc.gridx = 0;
//	        gbc.gridy = 0;
//	        gbc.gridwidth = 3;
//	        gbc.weightx = 1;
//	        gbc.gridheight = 1;
//	        gbc.fill = GridBagConstraints.HORIZONTAL;
//
//	        sliderPanel.add(opacitySlider, gbc);
//
//	        gbc.gridx = 0;
//	        gbc.gridy = 1;
//	        gbc.gridwidth = 1;
//	        gbc.weightx = 0;
//	        gbc.anchor = GridBagConstraints.WEST;
//	        gbc.fill = GridBagConstraints.NONE;
//
//	        sliderPanel.add(minimum, gbc);
//
//	        gbc.gridx = 1;
//	        gbc.anchor = GridBagConstraints.CENTER;
//	        gbc.weightx = .5;
//
//	        sliderPanel.add(current, gbc);
//
//	        gbc.gridx = 2;
//	        gbc.anchor = GridBagConstraints.EAST;
//	        gbc.weightx = 0;
//
//	        sliderPanel.add(maximum, gbc);
//	        sliderPanel.setBorder(buildTitledBorder("Opacity"));
//
//	        JPanel panelVOIProps = new JPanel(new GridBagLayout());
//	        panelVOIProps.setBorder(buildTitledBorder("VOI properties"));
//	        gbc.gridx = 0;
//	        gbc.gridy = 0;
//	        gbc.gridwidth = 1;
//	        gbc.gridheight = 1;
//	        gbc.weightx = 1;
//	        gbc.anchor = GridBagConstraints.WEST;
//	        gbc.fill = GridBagConstraints.HORIZONTAL;
//	        panelVOIProps.add(namePanel, gbc);
//	        gbc.gridy = 1;
//	        panelVOIProps.add(checkboxPanel, gbc);
//	        gbc.gridy = 2;
//	        gbc.weighty = 1;
//	        gbc.anchor = GridBagConstraints.NORTH; // gbc.fill = GridBagConstraints.BOTH;
//	        panelVOIProps.add(sliderPanel, gbc);
//
//	        listPanel = new PlugInStatisticsList();
//
//	        try {
//	            listPanel.setSliceCount(image.getExtents()[2]);
//	        } catch (ArrayIndexOutOfBoundsException aioobe) {
//	            // otherwise, this must be a 2d image.
//	            listPanel.setSliceCount(1);
//	        } finally {
//	            listPanel.setCheckBoxesEnabled();
//	        }
//
//	        checkboxExclude = new JCheckBox("Exclude intensity range");
//	        checkboxExclude.setFont(serif12);
//	        checkboxExclude.addActionListener(this);
//	        
//	        labelMin = new JLabel("Range: ");
//	        labelMin.setFont(serif12);
//	        labelMin.setForeground(Color.black);
//	        labelMin.setEnabled(false);
//
//	        textMin = new JTextField(5);
//	        textMin.setEnabled(false);
//	        textMin.setFont(serif12);
//
//	        labelMax = new JLabel(" to ");
//	        labelMax.setFont(serif12);
//	        labelMax.setForeground(Color.black);
//	        labelMax.setEnabled(false);
//
//	        textMax = new JTextField(5);
//	        textMax.setEnabled(false);
//	        textMax.setFont(serif12);
//	        
//	        checkboxSaveStats = new JCheckBox("Save statistics in header");
//	        checkboxSaveStats.setFont(serif12);
//	        //checkboxSaveStats.addActionListener(this);
//
//	        JPanel checkPanel = new JPanel(new GridBagLayout());
//	        GridBagConstraints gbc2 = new GridBagConstraints();
//	        gbc2.anchor = GridBagConstraints.WEST;
//	        gbc2.fill = GridBagConstraints.BOTH;
//	        gbc2.weightx = 1;
//	        gbc2.weighty = 1;
//	        gbc2.gridx = 0;
//	        gbc2.gridy = 0;
//	        checkPanel.add(checkboxExclude, gbc2);
//	        gbc2.gridy++;
//	        checkPanel.add(checkboxSaveStats, gbc2);
//
//	        JPanel rangePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
//
//	        rangePanel.add(labelMin);
//	        rangePanel.add(textMin);
//	        rangePanel.add(labelMax);
//	        rangePanel.add(textMax);
//
//	        JPanel intensityPanel = new JPanel();
//	        intensityPanel.setLayout(new BoxLayout(intensityPanel, BoxLayout.Y_AXIS));
//	        intensityPanel.add(checkPanel);
//	        intensityPanel.add(rangePanel);
//
//	        statsPanel = new JPanel(new BorderLayout());
//	        statsPanel.add(listPanel);
//	        statsPanel.add(intensityPanel, BorderLayout.SOUTH);
//
//	        JLabel labelSeed = new JLabel("Seed value (0-32K)");
//	        labelSeed.setFont(serif12);
//	        labelSeed.setForeground(Color.black);
//
//	        seedValueTF = new JTextField(5);
//	        seedValueTF.setFont(serif12);
//	        seedValueTF.addFocusListener(this);
//
//	        JPanel seedValuePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
//	        seedValuePanel.setBorder(buildTitledBorder("Watershed seed value"));
//	        seedValuePanel.add(labelSeed);
//	        seedValuePanel.add(seedValueTF);
//
//	        JPanel calcPanel = new JPanel(new BorderLayout());
//	        calcPanel.add(statsPanel);
//	        calcPanel.add(seedValuePanel, BorderLayout.SOUTH);
//
//	        applyButton = new JButton("Apply");
//	        applyButton.setPreferredSize(MipavUtil.defaultButtonSize);
//	        applyButton.setFont(serif12B);
//	        applyButton.addActionListener(this);
//
//	        cancelButton = buildCancelButton();
//	        cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
//	        
//	        helpButton = buildHelpButton();
//	        helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
//
//	        // build the VOI tree
//	        buildVOITree();
//	        buildVOIContourPane();
//
//	        GridBagConstraints gb = new GridBagConstraints();
//
//	        JPanel mainTreePanel = new JPanel(new GridBagLayout());
//	        mainTreePanel.setBorder(buildTitledBorder("VOI Browser"));
//
//	        gb.anchor = GridBagConstraints.CENTER;
//	        gb.gridx = 0;
//	        gbc.gridy = 0;
//	        gb.weightx = 1.0;
//	        gb.weighty = 1.0;
//	        gb.fill = GridBagConstraints.BOTH;
//
//	        mainTreePanel.add(voiTreePane, gb);
//
//	        JPanel treeOptionPanel = new JPanel(new BorderLayout());
//	        treeOptionPanel.setBorder(buildTitledBorder("Tree options"));
//	        followVOISelectionBox = new JCheckBox("Frame follows VOI selection", true);
//	        followVOISelectionBox.setFont(MipavUtil.font12);
//	        followVOISelectionBox.addActionListener(this);
//	        followVOISelectionBox.setEnabled(image.getNDims() > 2);
//	        treeOptionPanel.add(followVOISelectionBox, BorderLayout.CENTER);
//
//	        gb.gridy = 1;
//	        gb.weightx = 1;
//	        gb.weighty = 0;
//	        gb.fill = GridBagConstraints.HORIZONTAL;
//	        mainTreePanel.add(treeOptionPanel, gb);
//
//	        gb.gridy = 2;
//	        gb.weightx = .5;
//	        gb.weighty = .5;
//	        gb.fill = GridBagConstraints.BOTH;
//	        mainTreePanel.add(voiContourPane, gb);
//
//	        JPanel leftButton = new JPanel();
//	        leftButton.add(applyButton);
//	        leftButton.add(cancelButton);
//	        leftButton.add(helpButton);
//
//	        JPanel leftWholePanel = new JPanel(new BorderLayout());
//	        leftWholePanel.add(panelVOIProps, BorderLayout.NORTH);
//	        leftWholePanel.add(mainTreePanel, BorderLayout.CENTER);
//
//	        JPanel leftPanel = new JPanel(new BorderLayout());
//	        leftPanel.add(leftWholePanel);
//	        leftPanel.add(leftButton, BorderLayout.SOUTH);
//
//	        calcButton = new JButton("Calculate");
//	        calcButton.setPreferredSize(new Dimension(100, 30));
//	        calcButton.setFont(serif12B);
//	        calcButton.addActionListener(this);
//
//	        JPanel rightButton = new JPanel();
//	        rightButton.add(calcButton);
//	        //rightButton.add(helpButton);
//
//	        JPanel rightPanel = new JPanel(new BorderLayout());
//	        rightPanel.add(calcPanel);
//	        rightPanel.add(rightButton, BorderLayout.SOUTH);
//
//	        mainDialogPanel.setLayout(new GridBagLayout());
//	        gb.gridx = 0;
//	        gb.gridy = 0;
//	        gb.weightx = 1;
//	        gb.weighty = 1;
//	        gb.fill = GridBagConstraints.BOTH;
//	        mainDialogPanel.add(leftPanel, gb);
//
//	        gb.gridx = 1;
//	        mainDialogPanel.add(rightPanel, gb);
//
//
//	        // mainDialogPanel.setLayout(new BorderLayout());
//	        // mainDialogPanel.add(leftPanel, BorderLayout.WEST);
//	        // mainDialogPanel.add(rightPanel);
//	        mainDialogPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
//	        getContentPane().add(mainDialogPanel);
//	        pack();
//	    }
//	}
	
//	private class PlugInHandler extends VOIHandler {
//		
//		public PlugInHandler(ViewJComponentEditImage compImage) {
//			super(compImage);
//			voiDialog = new PlugInVOIStats(compImage.getFrame(), compImage.getActiveImage(), null);
//	        /*if (compImage.getFrame() != null) {
//	            voiDialog = new JDialogVOIStats(compImage.getFrame(), compImage.getActiveImage(), null);
//	            addVOIUpdateListener(voiDialog);
//	        }*/
//		}
//
//		public void deleteContour(VOI voi, int slice) {
//			// TODO Auto-generated method stub
//			ViewUserInterface.getReference().getMessageFrame().append("Working2\n", ViewJFrameMessage.DEBUG);
//			super.deleteContour(voi, slice);
//			ViewUserInterface.getReference().getMessageFrame().append("Working2\n", ViewJFrameMessage.DEBUG);
//		}
//
//		public void deleteSelectedVOI(boolean contoursOnly) {
//			// TODO Auto-generated method stub
//			ViewUserInterface.getReference().getMessageFrame().append("Working\n", ViewJFrameMessage.DEBUG);
//			super.deleteSelectedVOI(contoursOnly);
//			ViewUserInterface.getReference().getMessageFrame().append("Working\n", ViewJFrameMessage.DEBUG);
//		}
//
//		public void deleteVOIActivePt() {
//			// TODO Auto-generated method stub
//			super.deleteVOIActivePt();
//			ViewUserInterface.getReference().getMessageFrame().append("Working3\n", ViewJFrameMessage.DEBUG);
//		}
//
//		public void deleteVOIs() {
//			// TODO Auto-generated method stub
//			super.deleteVOIs();
//			ViewUserInterface.getReference().getMessageFrame().append("Working4\n", ViewJFrameMessage.DEBUG);
//		}
//	}
	
	/**
	 * Another extended MIPAV class.  Designed to allow for overwriting of VOIs without prompt during plugin. 
	 * Checks also exist to make sure coordinates are correctly written.   Used by saveAllVOIsTo(dir)
	 * 
	 * @author senseneyj
	 *
	 */
	private class PlugInFileVOI extends FileVOI {
		//~ Constructors ---------------------------------------------------------------------------------------------------

	    /**
	     * VOI reader/writer constructor.
	     *
	     * @param      fileName  file name
	     * @param      fileDir   file directory
	     * @param      image     image model: needed during the read process to ensure the VOI "fits" in the image space.
	     *
	     * @exception  IOException  if there is an error making the files
	     */
	    public PlugInFileVOI(String fileName, String fileDir, ModelImage image) throws IOException {
	    	super(fileName, fileDir, image);
	    }
	    
	    /**
	     * Writes VOI to an XML formatted file.
	     *
	     * @param   voi              VOI to be saved
	     * @param   saveAllContours  if true save all contours, not just the active ones
	     *
	     * @throws  IOException  exception thrown if there is an error writing the file
	     */
	    public void writeXML(VOI voi, boolean saveAllContours) throws IOException {
	    	
	    	if(file.exists() == true) {
                file.delete();
	    	}
	    	
	    	//now that file has been deleted, prompting will not occur
	    	super.writeXML(voi, saveAllContours);
	    }
	}
}
