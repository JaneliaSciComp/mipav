import WildMagic.LibFoundation.Mathematics.Vector2f;

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
import gov.nih.mipav.model.structures.event.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics;
import gov.nih.mipav.view.dialogs.JDialogVOIStats;
import gov.nih.mipav.view.dialogs.JDialogWinLevel;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics.JPanelStatisticsOptions;

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

public class PlugInMuscleImageDisplay extends ViewJFrameImage implements KeyListener, AlgorithmInterface {
    
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

    /** 
     * Denotes the anatomical part represented in the image. Implemented here in case 
     * this class is moved to its own class at a later time.
     */
    private ImageType imageType;

    /** Whether this image has mirror image muscles (eg VIEWS of thighs, abdomen. */
    private Symmetry symmetry;
    
    /**The dialog tabs that exist in this frame.*/
    private JTabbedPane dialogTabs;
    
    /**Currently open muscle tab, used to return from VOI tabs or Analysis tabs.*/
    private int activeTab;
    
    /**All tabs within this plugin. */
    private DialogPrompt[] tabs;
    
    /**The current slice of a 3D image.*/
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
    private Frame hiddenFrame;
    
    /** Whether the algorithm is being run in standAlone mode.*/
    private boolean standAlone;
    
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
     * Constructor for creating pluging within MIPAV system.
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
    	
    	ViewJProgressBar progressBar = new ViewJProgressBar("Automatic Seg", "Initializing...", 0, 100, true);
    	Vector <Frame> vec = userInterface.getImageFrameVector();
    	String fileName = image.getFileInfo()[0].getFileName();
    	for(int i=0; i<vec.size(); i++) {
    		if(!vec.get(i).getClass().equals(this.getClass())) {
    			if(vec.get(i) instanceof ViewJFrameImage) {
    				if(((ViewJFrameImage)vec.get(i)).getActiveImage().getFileInfo()[0].getFileName().equals(fileName)) {
    					hiddenFrame = vec.get(i);
    					hiddenFrame.setVisible(false);
    				}
    			}
    		}
    	}
    	setVisible(false);
        
        Preferences.setProperty(Preferences.PREF_CLOSE_FRAME_CHECK, String.valueOf(true));
        
        this.setImageA(image);
        this.setActiveImage(IMAGE_A);
        this.titles = titles;
        this.mirrorArr = new String[voiList.length][];
        this.noMirrorArr = new String[voiList.length][];
        this.calcTree = new TreeMap<String, Boolean>();
        this.voiBuffer = Collections.synchronizedMap(new TreeMap<String, PlugInSelectableVOI>());
        this.imageType = imageType;
        this.symmetry = symmetry;
        this.multipleSlices = multipleSlices;
        this.currentSlice = getViewableSlice();
        this.standAlone = false;
        
        //already added from super constructor
        //image.addImageDisplayListener(this);
        
        for(int i=0; i<voiList.length; i++) {
        	ArrayList<Comparable> mirrorArrList = new ArrayList<Comparable>(), noMirrorArrList = new ArrayList<Comparable>(), 
        				mirrorZList = new ArrayList<Comparable>(), noMirrorZList = new ArrayList<Comparable>();
        	for(int j=0; j<voiList[i].length; j++) {
        		voiBuffer.put(voiList[i][j].getName(), voiList[i][j]);
        		if(voiList[i][j].getName().contains("Left")) {
        			mirrorArrList.add(voiList[i][j].getName().substring(new String("Left ").length()));
        			mirrorZList.add(voiList[i][j].fillEligible());
        			calcTree.put(voiList[i][j].getName().substring(new String("Left ").length()), voiList[i][j].calcEligible());
        		} else if(voiList[i][j].getName().contains("Right")) {
        			//do nothing
        		} else {
        			noMirrorArrList.add(voiList[i][j].getName());
        			noMirrorZList.add(voiList[i][j].fillEligible());
        			calcTree.put(voiList[i][j].getName(), voiList[i][j].calcEligible());
        		}
        	}
        	mirrorArr[i] = new String[mirrorArrList.size()];
        	for(int j=0; j<mirrorArr[i].length; j++) 
        		mirrorArr[i][j] = (String)mirrorArrList.get(j);
        	noMirrorArr[i] = new String[noMirrorArrList.size()];
        	for(int j=0; j<noMirrorArr[i].length; j++) 
        		noMirrorArr[i][j] = (String)noMirrorArrList.get(j);
        }
        
        if (imageA == null) {
            return;
        }
        progressBar.updateValue(5);
        imageDir = getImageA().getFileInfo(getViewableSlice()).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
        
        File f;
        if(!(f = new File(imageDir+File.separator)).exists())
        	f.mkdir();

        System.out.println("Exists? "+f.exists());
        createVOIBuffer();
        
        initNext();
        
        //Automatic segmentation here
        setVisible(false);
        progressBar.setMessage("Creating algorithms...");
        progressBar.setSeparateThread(true);
        long time = System.currentTimeMillis();
        progressBar.updateValue(10);
        autoSegmentation();
        progressBar.updateValue(20);
    	progressBar.setMessage("Segmenting...");
        System.out.println("Time spent creating algs: "+(System.currentTimeMillis() - time));
        time = System.currentTimeMillis();
        waitAlg(progressBar);
        System.out.println("Total time spent waiting for threads: "+(System.currentTimeMillis() - time));
        progressBar.updateValue(90);
    	progressBar.setMessage("Displaying results...");
        initMuscleImage(2);
        getActiveImage().unregisterAllVOIs();
        initMuscleImage(1);
        getActiveImage().unregisterAllVOIs();
        initMuscleImage(0);
        progressBar.setVisible(false);
        progressBar.dispose();
        setVisible(true);
        
        Iterator<String> itr = voiBuffer.keySet().iterator();
        
        while(itr.hasNext()) {
        	PlugInSelectableVOI voi = voiBuffer.get(itr.next());
        	if(voi.calcEligible() && voi.area() > 0) {
        		long timeVOI = System.currentTimeMillis();
		        voi.setLastModified(timeVOI);
        	}
        }
        
        itr = voiBuffer.keySet().iterator();
        //always perform new calculation here, no need to check
        while(itr.hasNext()) {
        	PlugInSelectableVOI voi = voiBuffer.get(itr.next());
        	if(voi.calcEligible() && voi.area() > 0) {
		        MuscleCalculation muscleCalc = new MuscleCalculation(voi, voi.getName());
		        Thread calc = new Thread(calcGroup, muscleCalc, voi.getName());
		        calc.start();
        	}
        }
    }
    
    /**
     * Constructor for calling this as a stand-alone.  will invoke the class's own init() function
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
        
    	ViewJProgressBar progressBar = new ViewJProgressBar("Automatic Seg", "Initializing...", 0, 100, true);
    	setVisible(false);
    	progressBar.setVisible(true);
        this.titles = titles;
        this.mirrorArr = new String[voiList.length][];
        this.noMirrorArr = new String[voiList.length][];
        this.calcTree = new TreeMap<String, Boolean>();
        this.voiBuffer = new TreeMap<String, PlugInSelectableVOI>();
        this.standAlone = standAlone;
        
        createVOIBuffer();
        
        //create automatic VOIs here
        
        for(int i=0; i<voiList.length; i++) {
        	ArrayList<Comparable> mirrorArrList = new ArrayList<Comparable>(), noMirrorArrList = new ArrayList<Comparable>(), 
        				mirrorZList = new ArrayList<Comparable>(), noMirrorZList = new ArrayList<Comparable>();
        	for(int j=0; j<voiList[i].length; j++) {
        		voiBuffer.put(voiList[i][j].getName(), voiList[i][j]);
        		if(voiList[i][j].getName().contains("Left")) {
        			mirrorArrList.add(voiList[i][j].getName().substring(new String("Left ").length()));
        			mirrorZList.add(voiList[i][j].fillEligible());
        			calcTree.put(voiList[i][j].getName().substring(new String("Left ").length()), voiList[i][j].calcEligible());
        		} else if(voiList[i][j].getName().contains("Right")) {
        			//do nothing
        		} else {
        			noMirrorArrList.add(voiList[i][j].getName());
        			noMirrorZList.add(voiList[i][j].fillEligible());
        			calcTree.put(voiList[i][j].getName(), voiList[i][j].calcEligible());
        		}
        	}
        	mirrorArr[i] = new String[mirrorArrList.size()];
        	for(int j=0; j<mirrorArr[i].length; j++) 
        		mirrorArr[i][j] = (String)mirrorArrList.get(j);
        	noMirrorArr[i] = new String[noMirrorArrList.size()];
        	for(int j=0; j<noMirrorArr[i].length; j++) 
        		noMirrorArr[i][j] = (String)noMirrorArrList.get(j);
        }
        this.imageType = imageType;
        this.symmetry = symmetry;
        this.multipleSlices = multipleSlices;
        
        Preferences.setProperty(Preferences.PREF_CLOSE_FRAME_CHECK, String.valueOf(true));

    	initStandAlone();
    	setVisible(false);
    	this.setActiveImage(IMAGE_A);
    	scrollPane.requestFocus();
    	
    	ctMode(getImageA(), -175, 275);
    	
    	imageDir = getImageA().getFileInfo(getViewableSlice()).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
    	
    	if (imageA == null) {
            return;
        }
        progressBar.updateValue(5);
        
        File f;
        if(!(f = new File(imageDir+File.separator)).exists())
        	f.mkdir();

        System.out.println("Exists? "+f.exists());
        createVOIBuffer();
        
        //Automatic segmentation here
        
        progressBar.setMessage("Creating algorithms...");
        progressBar.setSeparateThread(true);
        long time = System.currentTimeMillis();
        progressBar.updateValue(10);
        autoSegmentation();
        progressBar.setVisible(true);
        progressBar.updateValue(20);
    	progressBar.setMessage("Segmenting...");
        System.out.println("Time spent creating algs: "+(System.currentTimeMillis() - time));
        time = System.currentTimeMillis();
        waitAlg(progressBar);
        System.out.println("Total time spent waiting for threads: "+(System.currentTimeMillis() - time));
        progressBar.updateValue(90);
    	progressBar.setMessage("Displaying results...");
        initMuscleImage(2);
        getActiveImage().unregisterAllVOIs();
        initMuscleImage(1);
        getActiveImage().unregisterAllVOIs();
        initMuscleImage(0);
        progressBar.setVisible(false);
        progressBar.dispose();
        setVisible(true);
        
        Iterator<String> itr = voiBuffer.keySet().iterator();
        
        while(itr.hasNext()) {
        	PlugInSelectableVOI voi = voiBuffer.get(itr.next());
        	if(voi.calcEligible() && voi.area() > 0) {
        		long timeVOI = System.currentTimeMillis();
		        voi.setLastModified(timeVOI);
        	}
        }
        
        itr = voiBuffer.keySet().iterator();
        //always perform new calculation here, no need to check
        while(itr.hasNext()) {
        	PlugInSelectableVOI voi = voiBuffer.get(itr.next());
        	if(voi.calcEligible() && voi.area() > 0) {
		        MuscleCalculation muscleCalc = new MuscleCalculation(voi, voi.getName());
		        Thread calc = new Thread(calcGroup, muscleCalc, voi.getName());
		        calc.start();
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
	        	System.out.println("Marrow seg alg finished");
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
	        	System.out.println("Bone seg alg finished");
	        }
	        if(thighSeg != null) {
	        	extend = (int)(System.currentTimeMillis() - time) / 2000 - 1;
	        	while(thighSeg.isAlive()) {
	        		if((System.currentTimeMillis() - time) / 2000 > extend) {
	        			progressBar.updateValue(70+(++extend));
	        		}
	        	}
	        	progressBar.setMessage("Thigh segmentation complete...");
	        	System.out.println("Thigh seg alg finished");
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
	        	System.out.println("Abdomen seg alg finished");
	        }
    	}
    }
    
    /**
     * Performs automatic segmentation for various images.
     */
    private void autoSegmentation() {
    	if(imageType.equals(ImageType.Thigh)) {
    		ModelImage srcImage = (ModelImage)getActiveImage().clone();
    		if(voiBuffer.get("Left Bone").area() == 0 && voiBuffer.get("Right Bone").area() == 0) {
		        ModelImage resultImage = (ModelImage)srcImage.clone();
		    	boneSeg = new PlugInAlgorithmCTBone(resultImage, srcImage, imageDir, voiBuffer.get("Left Bone").getColor());
		    	performSegmentation(boneSeg, resultImage);
    		}
	    	
    		if(voiBuffer.get("Left Marrow").area() == 0 && voiBuffer.get("Right Marrow").area() == 0) {
		    	ModelImage resultImage2 = (ModelImage)srcImage.clone();
		    	marrowSeg = new PlugInAlgorithmCTMarrow(resultImage2, srcImage, imageDir, voiBuffer.get("Left Marrow").getColor());
		    	performSegmentation(marrowSeg, resultImage2);
    		}
	    	
    		if(voiBuffer.get("Left Thigh").area() == 0 && voiBuffer.get("Right Thigh").area() == 0) {
		    	ModelImage resultImage3 = (ModelImage)srcImage.clone();
		    	thighSeg = new PlugInAlgorithmCTThigh(resultImage3, srcImage, imageDir, voiBuffer.get("Left Thigh").getColor());
		    	performSegmentation(thighSeg, resultImage3);
    		}
    	} else if(imageType.equals(ImageType.Abdomen)) {
    		ModelImage srcImage = (ModelImage)getActiveImage().clone();
    		if(voiBuffer.get("Abdomen").area() == 0) {
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
     * Handles automatic segmentation algorithm completions.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	
    		
		Vector<VOIBase>[] firstVOI = null;
		Vector<VOIBase>[] secondVOI = null;
		PlugInSelectableVOI firstBufferVOI = null;
		PlugInSelectableVOI secondBufferVOI = null;
    	if(algorithm instanceof PlugInAlgorithmCTThigh) {
			if(((PlugInAlgorithmCTThigh)algorithm).getLeftThighVOI() != null && 
					((PlugInAlgorithmCTThigh)algorithm).getRightThighVOI() != null) {
				System.out.println("Thigh VOIs completed correctly");
				firstVOI = ((PlugInAlgorithmCTThigh)algorithm).getLeftThighVOI().getCurves();
				secondVOI = ((PlugInAlgorithmCTThigh)algorithm).getRightThighVOI().getCurves();
				firstBufferVOI = voiBuffer.get("Left Thigh");
				secondBufferVOI = voiBuffer.get("Right Thigh");
				for(int i=0; i<firstVOI.length; i++) {
					for(int j=0; j<firstVOI[i].size(); j++) {
						firstBufferVOI.importCurve((VOIContour)firstVOI[i].get(j), i);
					}
					for(int j=0; j<secondVOI[i].size(); j++) {
						secondBufferVOI.importCurve((VOIContour)secondVOI[i].get(j), i);
					}
				}
				firstBufferVOI.setComputerGenerated(true);
				secondBufferVOI.setComputerGenerated(true);
			}
		} else if(algorithm instanceof PlugInAlgorithmCTMarrow) {
			if(((PlugInAlgorithmCTMarrow)algorithm).getLeftMarrowVOI() != null && 
					((PlugInAlgorithmCTMarrow)algorithm).getRightMarrowVOI() != null) {
				System.out.println("Marrow VOIs completed correctly");
				firstVOI = ((PlugInAlgorithmCTMarrow)algorithm).getLeftMarrowVOI().getCurves();
				secondVOI = ((PlugInAlgorithmCTMarrow)algorithm).getRightMarrowVOI().getCurves();
				firstBufferVOI = voiBuffer.get("Left Marrow");
				secondBufferVOI = voiBuffer.get("Right Marrow");
				for(int i=0; i<firstVOI.length; i++) {
					for(int j=0; j<firstVOI[i].size(); j++) {
						firstBufferVOI.importCurve((VOIContour)firstVOI[i].get(j), i);
					}
					for(int j=0; j<secondVOI[i].size(); j++) {
						secondBufferVOI.importCurve((VOIContour)secondVOI[i].get(j), i);
					}
				}
				firstBufferVOI.setComputerGenerated(true);
				secondBufferVOI.setComputerGenerated(true);
			}
		} else if(algorithm instanceof PlugInAlgorithmCTBone) {
			if(((PlugInAlgorithmCTBone)algorithm).getLeftBoneVOI() != null && 
					((PlugInAlgorithmCTBone)algorithm).getRightBoneVOI() != null) {
				System.out.println("Bone VOIs completed correctly");
				firstVOI = ((PlugInAlgorithmCTBone)algorithm).getLeftBoneVOI().getCurves();
				secondVOI = ((PlugInAlgorithmCTBone)algorithm).getRightBoneVOI().getCurves();
				firstBufferVOI = voiBuffer.get("Left Bone");
				secondBufferVOI = voiBuffer.get("Right Bone");
				for(int i=0; i<firstVOI.length; i++) {
					for(int j=0; j<firstVOI[i].size(); j++) {
						firstBufferVOI.importCurve((VOIContour)firstVOI[i].get(j), i);
					}
					for(int j=0; j<secondVOI[i].size(); j++) {
						secondBufferVOI.importCurve((VOIContour)secondVOI[i].get(j), i);
					}
				}
				firstBufferVOI.setComputerGenerated(true);
				secondBufferVOI.setComputerGenerated(true);
			}
		} else if(algorithm instanceof PlugInAlgorithmCTAbdomen) {
			if(((PlugInAlgorithmCTAbdomen)algorithm).getAbdomenVOI() != null) {
				System.out.println("Abdomen alg completed");
				firstVOI = ((PlugInAlgorithmCTAbdomen)algorithm).getAbdomenVOI().getCurves();
				firstBufferVOI = voiBuffer.get("Abdomen");
				for(int i=0; i<firstVOI.length; i++) {
					for(int j=0; j<firstVOI[i].size(); j++) {
						firstBufferVOI.importCurve((VOIContour)firstVOI[i].get(j), i);
					}
				}
				firstBufferVOI.setComputerGenerated(true);
			}
			if(((PlugInAlgorithmCTAbdomen)algorithm).getSubcutaneousVOI() != null) {
				secondVOI = ((PlugInAlgorithmCTAbdomen)algorithm).getSubcutaneousVOI().getCurves();
				secondBufferVOI = voiBuffer.get("Subcutaneous area");
				for(int i=0; i<secondVOI.length; i++) {
					for(int j=0; j<secondVOI[i].size(); j++) {
						secondBufferVOI.importCurve((VOIContour)secondVOI[i].get(j), i);
					}
				}
				secondBufferVOI.setComputerGenerated(true);
			}
		}

        if (algorithm != null) {
            algorithm.finalize();
            algorithm = null;
        }
        if(standAlone)
        	initControls();
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
    	
    	dialogTabs.removeAll();
    	dialogTabs = null;

    	System.gc();
    	
    	if (ViewUserInterface.getReference().isAppFrameVisible() == false &&
    			ViewUserInterface.getReference().getActiveImageFrame() == null) {
    		System.exit(0);
    	}
    	
    }
    
    @Override
    public void actionPerformed(ActionEvent e) {
        System.out.println(e);
    	String command = e.getActionCommand();
        //run through toggle buttons to see if a menu selected one (updates the button status)
        getControls().getTools().setToggleButtonSelected(command);
                
        if(command.equals(DialogPrompt.CHECK_VOI)) {
            ((VoiDialogPrompt)tabs[voiTabLoc]).setUpDialog(((JButton)(e.getSource())).getText());
            lockToPanel(voiTabLoc, "VOI"); //includes making visible
            initVoiImage(); //replacing current image and updating
        } else if(command.equals(DialogPrompt.CALCULATE)) {
        	lockToPanel(resultTabLoc, "Analysis"); //includes making visible
        	getActiveImage().unregisterAllVOIs();
	    	updateImages(true);
	    	boolean leftMarrow = false, rightMarrow = false, rightBone = false, leftBone = false, abdomen = false;
        	if((imageType.equals(ImageType.Thigh) && ((leftMarrow = !voiBuffer.get("Left Marrow").isCreated()) || 
        												(rightMarrow = !voiBuffer.get("Right Marrow").isCreated()) || 
        												(rightBone = !voiBuffer.get("Right Bone").isCreated()) || 
        												(leftBone = !voiBuffer.get("Left Bone").isCreated()))) || 
				(imageType.equals(ImageType.Abdomen) && ((abdomen = !voiBuffer.get("Abdomen").isCreated())))) {
        		String createStr = new String();
        		if(abdomen)
        			createStr += "\tAbdomen\n";
        		if(leftMarrow)
        			createStr += "\tLeft Marrow\n";
        		if(rightMarrow)
        			createStr += "\tRight Marrow\n";
        		if(leftBone)
        			createStr += "\tLeft Bone\n";
        		if(rightBone)
        			createStr += "\tRightBone\n";
        		MipavUtil.displayWarning("This tab calculates VOIs that depend on the following being created.\n"+
        				"Note that only muscle calculations will be correct.\n"+createStr);
        	}
        	((AnalysisPrompt)tabs[resultTabLoc]).setSlice(getViewableSlice());

        	((AnalysisPrompt)tabs[resultTabLoc]).enableCalcOutput();
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
        		initMuscleImage(activeTab);
        	} else if(command.equals(DialogPrompt.BACK)) {
        		unlockToPanel(resultTabLoc);
        		getActiveImage().unregisterAllVOIs();
        		initMuscleImage(activeTab);
        	} else if(command.equals(DialogPrompt.EXIT)) {
            	close();
        	} else if(command.equals(DialogPrompt.HELP)) {
        		if(imageType.equals(ImageType.Thigh))
        			MipavUtil.showHelp("MS00001");
        		else //image is of type abdomen
        			MipavUtil.showHelp("MS00050");
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
            } else if(reply == JOptionPane.YES_OPTION && !standAlone) {
            	//reshow the hidden frame
            	hiddenFrame.setVisible(true);
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
        tabs[resultTabLoc] = new AnalysisPrompt(this, mirrorArr, noMirrorArr);
        tabs[resultTabLoc].addListener(this);
        tabs[resultTabLoc].addComponentListener(this);
        tabs[resultTabLoc].setVisible(false);
                
        return panelA;
    }
    
    private void initNext() {
        
    	long time = System.currentTimeMillis();
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
        System.out.println("Done2: "+(System.currentTimeMillis()-time));
    }
    
    @Override
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
    	
    	zSlice = slice;
        controls.setZSlider(zSlice);
        updateImages(true);
        
        // livewire grad mag. should be recalculated for the new slice    
        if(currentSlice != slice) {
        	for(int i=0; i<tabs.length; i++) 
    			if(tabs[i].isVisible())
    				tabs[i].setSlice(slice);
        	currentSlice = slice;
        }
        
        componentImage.getVOIHandler().resetLivewire();
        setTitle();
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
     * Lock the dialog to a specific panel, such as VOI or Analysis
     * 
     * @param tabLoc the tab to open
     * @param title the title of the tab to open
     */
    public void lockToPanel(int tabLoc, String title) {
    	tabs[tabLoc].setVisible(true);
    	for(int i=0; i<voiTabLoc; i++)
    		dialogTabs.setEnabledAt(i, false);
    	dialogTabs.addTab(title, tabs[tabLoc]);
    	dialogTabs.setSelectedIndex(voiTabLoc);
    	updateImages(true);
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
     * Identifies which panel a particular VOI belongs to
     * 
     * @param name the VOI
     * @return a panel where 0 is first
     */
    public int getLocationStatus(String name) {
    	int loc = PlugInSelectableVOI.INVALID_LOC_NUMBER;
    	if(voiBuffer.get(name) != null)
    		loc = voiBuffer.get(name).getLocation();
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
    	if(voiBuffer.get(name) != null)
    		fill = voiBuffer.get(name).fillEligible();
    	return fill;
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
     * Initializes the VOI buttons for a particular pane.
     */
    private void initMuscleButtons(int pane) {
    	VOIVector vec = getActiveImage().getVOIs();
    	PlugInSelectableVOI temp = null;
    	for(int i=0; i<vec.size(); i++) 	
        	if((temp = voiBuffer.get(vec.get(i).getName())) != null && temp.getCurves()[getViewableSlice()].size() > 0) 
        		((MuscleDialogPrompt)tabs[pane]).setButton(temp);
    }
    
    /**
     * Initializes the image for a given pane, loads the relevant VOIs and updates the correct buttons.
     * @param pane the current pane
     */
    private void initMuscleImage(int pane) {        
    	componentImage.setCursorMode(ViewJComponentBase.NEW_VOI);
    	getVOIs(pane);
        
        System.out.println("Active tab: :"+activeTab);
    	initMuscleButtons(pane);
        
    	this.repaint();

        updateImages(true);
    }
    
    /**
     * Initializes the voiBuffer at program creation by loading all VOIs from the
     * file system for the first time.
     */
    private void createVOIBuffer() {
    	Iterator<String> voiItr = voiBuffer.keySet().iterator();
    	
    	while(voiItr.hasNext()) {
    		String name;
    		PlugInSelectableVOI v = loadVOI(name = voiItr.next());
    		
    		Color c = PlugInSelectableVOI.INVALID_COLOR;
			
			if((c = v.getColor()).equals(PlugInSelectableVOI.INVALID_COLOR)) {
            	if((c = hasColor(v)).equals(PlugInSelectableVOI.INVALID_COLOR))
                    v.setColor(c = colorPick[colorChoice++ % colorPick.length]);
            	else
            		v.setColor(c);
			} else
				v.setColor(c);
			voiBuffer.put(name, v);
			System.out.println(v.getName() +" "+ v.getColor());
    	}
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
	    		v.setThickness(2);
	    		v.setDisplayMode(VOI.BOUNDARY);
    			getActiveImage().registerVOI(v);
	    	}
    	}
    }
    
    /**
     * Initializes the image of the a voiDialogPrompt by setting relevant VOIs to
     * solid mode, note does not retrieve VOIs from the buffer 
     */
    private void initVoiImage() {
    	//VOIs of pane already loaded, just need to make relevant ones solid
        VOIVector voiVec = getActiveImage().getVOIs();
        VOI voi;
    	for(int i=0; i<voiVec.size(); i++) 
    		if(voiBuffer.get((voi = voiVec.get(i)).getName()).fillEligible()) 
    			if(!(((VoiDialogPrompt)tabs[voiTabLoc]).getObjectName().equals(voi.getName()))) 
    				voi.setDisplayMode(VOI.SOLID);
        
        componentImage.setCursorMode(ViewJComponentBase.NEW_VOI);
        updateImages(true);
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
    	public static final String OUTPUT_ALL = "Output All";
    	public static final String TOGGLE_LUT = "Toggle LUT";
    	public static final String OUTPUT = "Output";
    	public static final String BACK = "Back";
    	public static final String RESET = "Reset";
    	public static final String HIDE_ONE = "Hide except";
    	
    	/** default button list*/
    	protected String buttonStringList[] = {OK, HIDE_ALL, HELP};
    	
    	/** The group of buttons in this dialog prompt*/
    	protected JButton buttonGroup[];
    	
    	/**The containing frame*/
    	protected PlugInMuscleImageDisplay muscleFrame;
    	
    	/**The ActionListeners of a given initialization of this class*/
    	private Vector<ActionListener> objectList = new Vector<ActionListener>();
    	
    	/**The title of this DialogPrompt*/
    	protected String title;
    	
    	protected boolean completed = false;
    	
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
    	 * Returns the current title of this dialog prompt.
    	 */
    	public String getTitle() {
    		return title;
    	}
    	
    	public boolean completed() {
    		return completed;
    	}
    	
    	/**
    	 * Replaces buttonStringList with the given buttons.  
    	 * @param buttonString
    	 */
    	protected void setButtons(String[] buttonString) {
    		this.buttonStringList = buttonString;
    	}
    	
    	/**
    	 * Abstract method required for initializing display.
    	 */
    	protected abstract void initDialog();
    	
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
         * Add a listener to this class so that when when the dialog has completed processing it can use notifyListener
         * to notify all listeners that the dialog has completed.
         *
         * @param  obj  AlgorithmInterface "object' to be added to the list
         */
        public void addListener(ActionListener obj) {
        	objectList.addElement(obj);
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
            	for(int i=0; i<voiPromptBufferAppend.size(); i++)
            		voiPromptBuffer.add(voiPromptBufferAppend.get(i));
                getActiveImage().unregisterAllVOIs();
                ((JButton)e.getSource()).setText(SHOW_ALL);
                ((JButton)e.getSource()).setActionCommand(SHOW_ALL);
                updateImages(true);
            } else if(command.equals(SHOW_ALL)) {
            	//show all VOIs previously drawn, do not get rid of any VOIs on screen
            	VOIVector voiPromptBufferAppend = (VOIVector)getActiveImage().getVOIs().clone();
            	for(int i=0; i<voiPromptBufferAppend.size(); i++)
            		voiPromptBuffer.add(voiPromptBufferAppend.get(i));
        		getActiveImage().unregisterAllVOIs();
            	for(int i=0; i<voiPromptBuffer.size(); i++)
            		getActiveImage().registerVOI(voiPromptBuffer.get(i));
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
                        if(voiBuffer.get(objectName).calcEligible()) {
	                        MuscleCalculation muscleCalc = new MuscleCalculation(goodVoi, objectName);
	                        Thread calc = new Thread(calcGroup, muscleCalc, objectName);
	                        calc.start();
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
	                    if(goodVoi != null)
	                    	getActiveImage().registerVOI(goodVoi);	                    
	                	updateImages(true);
                	}
                	
                } else if (command.equals("PropVOIUp")) {
                	performSnake(AlgorithmSnake.PROP_NEXT);
                	incSlice();
                } else if (command.equals("PropVOIDown")) {
                	performSnake(AlgorithmSnake.PROP_PREV);
                	decSlice();
                } else if (command.equals("PropVOIAll")) {
                	performSnake(AlgorithmSnake.PROP_ALL);
                } 
            }
            
        /**Method for dealing with algorithms*/
        public void algorithmPerformed(AlgorithmBase algorithm) {
			if(algorithm instanceof AlgorithmSnake) {
				snakeProgress.updateValue(75);
				System.out.println("Performed successfully");
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
         * Prepares to leave voi dialog, clears the voiPromptBuffer.
         */
        public void takeDownDialog() {
        	
        	removeAll();
        	voiPromptBuffer.removeAllElements();
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
        	
        	voiExists = voiBuffer.get(objectName).isCreated();
        	if(voiExists)
        		voiInvestigated = ((PlugInSelectableVOI)voiBuffer.get(objectName).clone());
        	
        	for(int i=0; i<buttonGroup.length; i++) {
	        	if(buttonGroup[i].getText().contains(HIDE_ONE)) {
	        		buttonGroup[i].setText(HIDE_ONE+" "+name.toLowerCase());
	        		buttonGroup[i].setPreferredSize(new Dimension(185, 30));
	        	}
	        }
            
            updateSelectionLabel();
            
            if(voiExists && voiBuffer.get(name).isComputerGenerated()) 
            	createWarningLabel();
        }
        
        /**
         * @return the current VOI
         */
        public String getObjectName() {
            return objectName;
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
                    if(srcVOI.get(i).getCurves()[getViewableSlice()].size() > 0) {
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
            
            
            Vector<VOIBase>[] curves = goodVOI.getCurves();           
       
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
            
            
        	for(int i=0; i<curves.length; i++) { 
        		for(int j=0; j<curves[i].size(); j++) {
        			if(curveReplace) 
        				voiBuffer.get(objectName).removeCurve(j, i);
        		    voiBuffer.get(objectName).importCurve((VOIContour)curves[i].get(j), i);
        		}
        	}
            
            return voiBuffer.get(objectName);
        }

		@Override
		public void setSlice(int slice) {
			//Do nothing since this dialog does not depend on a changing slice (though could in the future)
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
        
        /**The location of this dialog in the parentFrame's array*/
        public int getIndex() {
        	return index;
        }
        
        //TODO: Put PlugInMuscleImageDisplay's actions here
        //calculate, help, exit
        public void actionPerformed(ActionEvent e) {
            System.err.println(e.getActionCommand());
            System.out.println(e.getActionCommand());
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
        
        /**
         * Implemented abstract method for dealing with slice changes, currently sets check boxes of
         * VOIs that have a curve on the given slice to that VOIs color.
         */
        public void setSlice(int slice) {
        	PlugInSelectableVOI temp;
        	for(int i=0; i<mirrorCheckArr.length; i++) {
        		if((temp = voiBuffer.get(mirrorButtonArr[i].getText())).getCurves()[slice].size() == 0) {
	        		mirrorCheckArr[i].setColor(Color.black);
	        		mirrorCheckArr[i].getColorButton().colorChanged(Color.black);
        		} else {
        			mirrorCheckArr[i].setColor(temp.getColor());
	        		mirrorCheckArr[i].getColorButton().colorChanged(temp.getColor());
        		}
        		//System.out.println("Size of "+temp.getName()+": "+temp.getCurves()[slice].size());
        	}
        	for(int i=0; i<noMirrorCheckArr.length; i++) {
        		if((temp = voiBuffer.get(noMirrorButtonArr[i].getText())).getCurves()[slice].size() == 0) {
	        		noMirrorCheckArr[i].setColor(Color.black);
	        		noMirrorCheckArr[i].getColorButton().colorChanged(Color.black);
        		} else {
        			noMirrorCheckArr[i].setColor(temp.getColor());
	        		noMirrorCheckArr[i].getColorButton().colorChanged(temp.getColor());
        		}	
        		//System.out.println("Size of "+temp.getName()+": "+temp.getCurves()[slice].size());
        	}
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

                mirrorCheckArr[i] = new ColorButtonPanel(Color.BLACK, mirrorButtonArr[i].getText());        
               
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
        			if(v.isComputerGenerated())
        				mirrorButtonArr[i].setForeground(Color.RED);
        			else
        				mirrorButtonArr[i].setForeground(Color.BLACK);
        		}
        	}
        	for(int i=0; i<noMirrorButtonArr.length; i++) {
        		if(name.equals(noMirrorButtonArr[i].getText())) {
        			noMirrorCheckArr[i].setColor(c);
        			v.addVOIListener(noMirrorCheckArr[i].getColorButton());
        			if(v.isComputerGenerated())
        				noMirrorButtonArr[i].setForeground(Color.RED);
        			else
        				noMirrorButtonArr[i].setForeground(Color.BLACK);
        		}
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
        
        /**
         * Gets the symmetric buttons in this panel.
         */
        public JButton[] getMirrorButton() {
        	if(mirrorButtonArr != null)
        		return mirrorButtonArr;
        	return new JButton[0];
        }
        
        /**
         * Gets the non-symmetric buttons in this panel.
         */
        public JButton[] getNoMirrorButton() {
        	if(noMirrorButtonArr != null)
        		return noMirrorButtonArr;
        	return new JButton[0];
        }
    }
    
    /**
     * This extension of DialogPrompt displays performed calculations.
     * 
     * @author senseneyj
     *
     */
    private class AnalysisPrompt extends DialogPrompt implements ActionListener {
		
    	//~ Static fields/initializers -------------------------------------------------------------------------------------
    	
    	public static final String LOAD_VOI = "Load VOI";
    	
    	private final String[] buttonStringList = {OUTPUT, OUTPUT_ALL, SAVE, TOGGLE_LUT, HELP, BACK};
	
    	//~ Instance fields -------------------------------------------------------------------------------------
    	
    	/** Labels for instructions. */
		private JLabel[] instructionLabel;

		/** Text for muscles where a mirror muscle may exist. */
		private String[][] mirrorArr;

		/** Text for muscles where mirror muscles are not considered. */
		private String[][] noMirrorArr;
		
		/** Side check box for all symmetric objects. */
		private ColorButtonPanel[][] mirrorCheckArr;
		
		/** Side check box for all non-symmetric objects. */
		private ColorButtonPanel[][] noMirrorCheckArr;
		
		/** Buttons for all symmetric objects. */
		private JButton[][] mirrorButtonArr;
		
		/** Buttons for all non-symmetric objects. */
		private JButton[][] noMirrorButtonArr;
		
		/**Seed for random color chooser for VOIs that have not had a color assigned to them. */
		private int colorChoice = 0;
		
		/**Text output generated after hitting SAVE*/
		private CustomOutput ucsdOutput = new CustomOutput();
		
		/**Whether the custom lookup table is being displayed*/
		private boolean lutOn = false;
		
		/**A mapping of names to color panels for easy referencing. */
		private TreeMap<String,ColorButtonPanel> checkBoxLocationTree;
	
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
	        
	        setButtons(buttonStringList);

	        //by looking at calculations up here, this process can be done independent
	        //of the multiple symmetries that may exist within a given body.
	        this.noMirrorArr = getCalcItems(noMirrorArr);
	        this.mirrorArr = getCalcItems(mirrorArr);
	        
	        checkBoxLocationTree = new TreeMap<String, ColorButtonPanel>();

	        colorChoice = 0;
	        
	        initDialog();
	    }
		
		/**
		 * Gets all items (symmetric and non-symmetric) that are calulated.
		 */
		private String[][] getCalcItems(String[][] objectArr) {
			String[][] resultArr = new String[objectArr.length][];
			String tempStr = "";
			for(int i=0; i<objectArr.length; i++) {
				ArrayList<String> tempObj = new ArrayList<String>();
				for(int j=0; j<objectArr[i].length; j++) 
					if(calcTree.get(tempStr = objectArr[i][j]).equals(true))
						tempObj.add(tempStr);
				resultArr[i] = new String[tempObj.size()];
				for(int j=0; j<resultArr[i].length; j++)
					resultArr[i][j] = tempObj.get(j);
			}
			return resultArr;
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
					checkBoxLocationTree.get(it.next()).setColor(Color.BLACK);
				}
			}
			boolean voiExists = false;
			for(int index=0; index<mirrorButtonArr.length; index++) {
				for(int i=0; i<mirrorButtonArr[index].length; i++) {
					mirrorButtonArr[index][i].setEnabled(voiExists = voiExists(mirrorButtonArr[index][i].getText(), slice));
					mirrorButtonArr[index][i].setForeground(Color.BLACK);
					if(voiExists && voiBuffer.get(mirrorButtonArr[index][i].getText()).isComputerGenerated())
						mirrorButtonArr[index][i].setForeground(Color.RED);
				}
				for(int i=0; i<noMirrorButtonArr[index].length; i++) {
					noMirrorButtonArr[index][i].setEnabled(voiExists(noMirrorButtonArr[index][i].getText(), slice));
					noMirrorButtonArr[index][i].setForeground(Color.BLACK);
					if(voiExists && voiBuffer.get(noMirrorButtonArr[index][i].getText()).isComputerGenerated())
						noMirrorButtonArr[index][i].setForeground(Color.RED);
				}
			}
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
		 * Removes the CT Thigh specific lut
		 */
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
		
		/**
		 * Initializes the instruction panel.
		 */
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
		
		/**
		 * Initializes the symmetric buttons from a particular pane.
		 */
		private JPanel initSymmetricalObjects(int index) {

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
	    
		/**
		 * Initializes the non-symmetric buttons from a particular pane.
		 */
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
	    
	    /**
         * Implementing abstract method to handle calculating buttons
         */
	    public void actionPerformed(ActionEvent e) {
	    	System.out.println("Caught 2: "+e.getActionCommand());
	    	String command = e.getActionCommand();
	        if(command.equals(HIDE_ALL)) {
	            //clear all VOIs drawn
	        	muscleFrame.getImageA().unregisterAllVOIs();
	        	muscleFrame.updateImages();
	        } else {
	
	            if (command.equals(OUTPUT)) {
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
			if(calcGroup.activeCount() > 0) {
				//TODO: Check thread manager here
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
			
			//if PDF hasnt been created and we're saving, create it now
			if (doSave && !pdfCreated) {
				createPDF();
				pdfCreated = true;
			}
			Iterator<String> itr;
			if(all)
				itr = voiBuffer.keySet().iterator();
			else {
				ArrayList<String> totalList = new ArrayList<String>(), subList = new ArrayList<String>();
				for (int listNum = 0; listNum < mirrorButtonArr.length; listNum++, subList = new ArrayList<String>())  {   	
	    			for(int i=0; i<mirrorButtonArr[listNum].length; i++) 
	    				if(mirrorCheckArr[listNum][i].isSelected())
	    					subList.add(mirrorButtonArr[listNum][i].getText());
	    			totalList.addAll(subList);
		    	}
				for (int listNum = 0; listNum < noMirrorButtonArr.length; listNum++, subList = new ArrayList<String>())  {   	
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
				PlugInSelectableVOI temp;
				if((temp = voiBuffer.get(itrObj)) != null && temp.getMeanFatH() != PlugInSelectableVOI.NOT_CALC) {
					
					totalAreaCalc = temp.getTotalAreaCalc();
					totalAreaCount = temp.getTotalAreaCount();
					fatArea = temp.getFatArea();
					leanArea = temp.getLeanArea();
					meanFatH = temp.getMeanFatH();
					meanLeanH = temp.getMeanLeanH();
					meanTotalH = temp.getMeanTotalH();
					
					System.out.println("Compare areas of "+temp.getName()+": "+totalAreaCalc+"\tcount: "+totalAreaCount);
					
					if (doSave) {
						addToPDF((String)itrObj, fatArea, leanArea, totalAreaCount, meanFatH, meanLeanH, meanTotalH, wholeTable);
						if(multipleSlices) {
							for(int i=0; i<getActiveImage().getExtents()[2]; i++) {
								totalAreaCount = temp.getTotalAreaCount(i);
								fatArea = temp.getFatArea(i);
								leanArea = temp.getLeanArea(i);
								meanFatH = temp.getMeanFatH(i);
								meanLeanH = temp.getMeanLeanH(i);
								meanTotalH = temp.getMeanTotalH(i);
								
								addToPDF((String)itrObj, fatArea, leanArea, totalAreaCount, meanFatH, meanLeanH, meanTotalH, sliceTable[i]);
							}
						}
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
					    		
				Thread output = new Thread(ucsdOutput);
		    	output.start();
				
				//now load all VOIs at once:
				ArrayList<String> totalList = new ArrayList<String>(), subList = new ArrayList<String>();
				for (int listNum = 0; listNum < mirrorButtonArr.length; listNum++, subList = new ArrayList<String>())  {   	
	    			for(int i=0; i<mirrorButtonArr[listNum].length; i++) 
	    				if(mirrorButtonArr[listNum][i].isEnabled())
	    					subList.add(mirrorButtonArr[listNum][i].getText());
	    			totalList.addAll(subList);
		    	}
				for (int listNum = 0; listNum < noMirrorButtonArr.length; listNum++, subList = new ArrayList<String>())  {   	
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
		
		/**
		 * Enables buttons that rely on calculating being completed.
		 */
		public void enableCalcOutput() {
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
	    
		/**
		 * Generates tab-delimited text output.
		 * 
		 * @author senseneyj
		 *
		 */
		private class CustomOutput implements Runnable {
			
			/** Whether this Runnable has completed*/
			private boolean done = false;
			
			/**
			 * Produces output into text file in NIA_Seg
			 */
			public void run() {
				long time = System.currentTimeMillis();
				ArrayList<PlugInSelectableVOI> calcList = new ArrayList<PlugInSelectableVOI>();
				Iterator<String> tempItr = voiBuffer.keySet().iterator();
				PlugInSelectableVOI temp = null;
				while(tempItr.hasNext()) {
					if((temp = voiBuffer.get(tempItr.next())).calcEligible())
						calcList.add(temp);
				}
				
				String fileDir = imageDir;
				String fileName = "Text_Report.txt";
				textFile = new File(fileDir + File.separator + fileName);
				if(textFile.exists()) {
					int i=0;
					while(textFile.exists() && i<1000) {
						fileName = "Text_Report-"+(++i)+ ".txt";
						if(i == 1000) 
							MipavUtil.displayError("Too many text documents have been created, overwriting "+fileName);
						textFile = new File(fileDir + File.separator + fileName);
					}
				}
				System.out.println("Text path: "+textFile.getAbsolutePath());
				
				String[] output = assembleOutput();
				
				try {
					BufferedWriter writer = new BufferedWriter(new FileWriter(textFile));
					
					//write output
					for(int i=0; i<output.length; i++) {
						writer.write(output[i]);
						writer.newLine();
					}
					
					writer.close();
				} catch(IOException e) {
					System.err.println("Error creating, writing or closing ucsd file.");
					e.printStackTrace();
					return;
				}
				System.out.println("Time for output: "+(System.currentTimeMillis() - time));
				done = true;
			}
			
			/**
			 * Assembles output for all slices of image.
			 * @return array of slice data
			 */
			private String[] assembleOutput() {
				String[] sliceStr;
				if(getActiveImage().getExtents().length > 2)
					sliceStr = new String[getActiveImage().getExtents()[2]];
				else
					sliceStr = new String[1];
				
				for(int i=0; i<sliceStr.length; i++) {
					sliceStr[i] = new String();
					
					//insert static elements
					FileInfoDicom fileInfo = (FileInfoDicom)getActiveImage().getFileInfo()[0];
					DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
					Date date = new Date();
					DecimalFormat dec = new DecimalFormat("0.0000");
					//patient id
					String id = (String)fileInfo.getTagTable().getValue("0010,0020");
					sliceStr[i] += id != null ? id.trim() : "0"+"\t";
					//slice number
					sliceStr[i] += Integer.toString(i)+"\t";
					//scan date
					String dateStr = (String)fileInfo.getTagTable().getValue("0008,0020");
					sliceStr[i] += dateStr != null ? dateStr.trim() : "0"+"\t";
					//center
					String center = (String)fileInfo.getTagTable().getValue("0008,0080");
					sliceStr[i] += center != null ? center.trim() : "0"+"\t";
					//analysis date
					sliceStr[i] += dateFormat.format(date)+"\t";
					//analyst
					sliceStr[i] += System.getProperty("user.name")+"\t";
					//pixel size
					sliceStr[i] += dec.format(getActiveImage().getResolutions(0)[0]*.1)+"\t";
					//slice thickness (mm)
					sliceStr[i] += dec.format(getActiveImage().getFileInfo()[getViewableSlice()].getSliceThickness())+"\t";
					//table height (cm)
					String heightUnformat = (String)fileInfo.getTagTable().getValue("0018,1130");
					String height = heightUnformat != null ? dec.format(Double.valueOf(heightUnformat.trim())) : "0";
					sliceStr[i] += height+"\t";
					
					//insertCalculations
					PlugInSelectableVOI temp = null;
					ArrayList<PlugInSelectableVOI> calcItems = new ArrayList<PlugInSelectableVOI>(voiBuffer.keySet().size());
					Iterator<PlugInSelectableVOI> firstItr = voiBuffer.values().iterator();
					while(firstItr.hasNext()) {
						if((temp = firstItr.next()).calcEligible())
							calcItems.add(temp);
					}
					ArrayList<PlugInSelectableVOI> orderedCalcItems = (ArrayList<PlugInSelectableVOI>)calcItems.clone();
					for(int j=0; j<calcItems.size(); j++) 
						orderedCalcItems.set(calcItems.get(j).getOutputLoc(), calcItems.get(j));
					dec = new DecimalFormat("0.##");
					for(int j=0; j<orderedCalcItems.size(); j++) {
						temp = orderedCalcItems.get(j);
						if(temp.isCreated()) {
							sliceStr[i] += dec.format(temp.getTotalAreaCount(i))+"\t";
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
			
			/**
			 * Whether the output has finished.
			 */
			public boolean isFinished() {
				return done;
			}
		}
	}
    /**
	 * Performs required calculations for plugin. Partial voluming error ~2%
	 * 
	 * @author senseneyj
	 *
	 */
	private class MuscleCalculation implements Runnable {
		
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
		private VOI currentVOI;
		
		/**Whether the Runnable has completed.*/
		private boolean done = false;
		
		/**The name of the current VOI being calculated.*/
		private String name;
		
		/**
		 * 
		 * @param newVOI the VOI to work on
		 * @param name the name of the VOI
		 */
		public MuscleCalculation(VOI newVOI, String name) {
			super();
			this.currentVOI = newVOI;
			this.name = name;
		}
		
		/**
		 * Performs all calculations for a particular VOI.
		 */
		public void run() {
			done = false;
			//ViewJProgressBar progressBar = new ViewJProgressBar("Calculations", "Initializing...", 0, 100, true);
			long time = System.currentTimeMillis();
			
			ArrayList<PlugInSelectableVOI> residuals = new ArrayList<PlugInSelectableVOI>();
			VOI v = currentVOI;
			double wholeMultiplier = 0.0, sliceMultiplier = 0.0;
			wholeMultiplier = sliceMultiplier = Math.pow(getActiveImage().getResolutions(0)[0]*0.1, 2);
			if(multipleSlices)
				wholeMultiplier *= (getActiveImage().getResolutions(0)[2]*0.1);
			System.out.println("Whole Multiplier: "+wholeMultiplier+"\tSliceMultiplier: "+sliceMultiplier);
			PlugInSelectableVOI temp = voiBuffer.get(name);
			residuals = getResiduals(temp);
			ArrayList<Thread> calc = new ArrayList<Thread>(residuals.size());
			Thread tempThread = null;
			for(int i=0; i<residuals.size(); i++) {
				if(residuals.get(i).getLastCalculated() == null || 
						residuals.get(i).getLastCalculated().compareTo(residuals.get(i).getLastModified()) < 0) {
					MuscleCalculation muscleCalc = new MuscleCalculation(residuals.get(i), residuals.get(i).getName());
		            //No thread group this time since just joining all later
					calc.add(tempThread = new Thread(muscleCalc));
		            tempThread.start();
				} else
					System.out.println("Just avoided calculating "+residuals.get(i).getName());
			}
			long time2 = System.currentTimeMillis();
			System.out.println("Waiting for threads to complete");
			try {
				for(int i=0; i<calc.size(); i++)
					calc.get(i).join();
			} catch(InterruptedException e) {
				System.err.println("Algorithm failed, calculations may be inaccurate.");
				e.printStackTrace();
			}
			System.out.println("Time spent waiting: "+(System.currentTimeMillis() - time2)+" so that "+name+" can finish.");
			//note that even for 3D images this will still be called area, even though refers to volume
			double fatArea = getPieceCount(v, FAT_LOWER_BOUND, FAT_UPPER_BOUND)*wholeMultiplier;
			double partialArea = getPieceCount(v, FAT_UPPER_BOUND, MUSCLE_LOWER_BOUND)*wholeMultiplier; 
			double leanArea = getPieceCount(v, MUSCLE_LOWER_BOUND, MUSCLE_UPPER_BOUND)*wholeMultiplier; 
			double totalAreaCalc = getTotalAreaCalc(v)*wholeMultiplier;
			double totalAreaCount = getTotalAreaCount(v)*wholeMultiplier;
			double fatAreaLarge = fatArea, leanAreaLarge = leanArea, totalAreaLarge = totalAreaCalc;
			//corrected area = abs(oldArea - sum(residual areas))
			for(int j=0; j<residuals.size(); j++) {
				fatArea = Math.abs(fatArea - residuals.get(j).getFatArea());
				partialArea = Math.abs(partialArea - residuals.get(j).getPartialArea());
				leanArea = Math.abs(leanArea - residuals.get(j).getLeanArea());
				totalAreaCalc = Math.abs(totalAreaCalc - residuals.get(j).getTotalAreaCalc());
				totalAreaCount = Math.abs(totalAreaCount - residuals.get(j).getTotalAreaCount());
			}
			
			temp.setFatArea(fatArea);
			temp.setPartialArea(partialArea);
			temp.setLeanArea(leanArea);
			temp.setTotalAreaCalc(totalAreaCalc);
			temp.setTotalAreaCount(totalAreaCount);
			
			double meanFatH = getMeanH(v, FAT_LOWER_BOUND, FAT_UPPER_BOUND);// + OFFSET;
			double meanLeanH = getMeanH(v, MUSCLE_LOWER_BOUND, MUSCLE_UPPER_BOUND);// + OFFSET;
		    double meanTotalH = getMeanH(v, FAR_LOWER_BOUND, FAR_UPPER_BOUND);// + OFFSET;
		    double meanFatHResidual = 0, meanLeanHResidual = 0, meanTotalHResidual = 0;
		    
		    //corrected mean = abs(oldMean*oldArea - sum(residualMean*residualArea))/abs(oldArea - sum(residualArea))
		    for(int j=0; j<residuals.size(); j++) {
		    	meanFatHResidual += residuals.get(j).getMeanFatH()*residuals.get(j).getFatArea();
		    	meanLeanHResidual += residuals.get(j).getMeanLeanH()*residuals.get(j).getLeanArea();
		    	meanTotalHResidual += residuals.get(j).getMeanTotalH()*residuals.get(j).getTotalAreaCalc();
		    }
		    
		    meanFatH = (meanFatH*fatAreaLarge - meanFatHResidual) / fatArea;
		    meanLeanH = (meanLeanH*leanAreaLarge - meanLeanHResidual) / leanArea;
		    meanTotalH = (meanTotalH*totalAreaLarge - meanTotalHResidual) / totalAreaCalc;
		    
		    //sign errors result from adding areas that should've been subtracted
		    if(meanFatH > 0) {
		    	meanFatH = -meanFatH;
		    	meanTotalH = -meanTotalH;
		    } else if(new Double(meanFatH).equals(Double.NaN)) 
		    	meanFatH = 0;
		    if(meanLeanH < 0)
		    	meanLeanH = -meanLeanH;
		    else if(new Double(meanLeanH).equals(Double.NaN))
		    	meanLeanH = 0;
		    if(new Double(meanTotalH).equals(Double.NaN))
		    	meanTotalH = 0;
		    
		    temp.setMeanFatH(meanFatH);
			temp.setMeanLeanH(meanLeanH);
			temp.setMeanTotalH(meanTotalH);
			
			System.out.println("Number of slices: "+temp.getZDim());
			for(int k=0; k<temp.getZDim(); k++) {
				//progressBar.setMessage("Calculating "+name.toLowerCase()+" slice "+k+"...");
				VOI v2 = (VOI)v.clone();
				for(int n=0; n<temp.getZDim(); n++) {
					if(n != k)
						v2.removeCurves(n);
				}
    			residuals = getResiduals(temp);
    			//note that even for 3D images this will still be called area, even though refers to volume
    			fatArea = getPieceCount(v2, FAT_LOWER_BOUND, FAT_UPPER_BOUND)*sliceMultiplier;
    			partialArea = getPieceCount(v2, FAT_UPPER_BOUND, MUSCLE_LOWER_BOUND)*sliceMultiplier; 
    			leanArea = getPieceCount(v2, MUSCLE_LOWER_BOUND, MUSCLE_UPPER_BOUND)*sliceMultiplier; 
    			totalAreaCalc = getTotalAreaCalc(v2)*sliceMultiplier;
    			totalAreaCount = getTotalAreaCount(v2)*sliceMultiplier;
    			fatAreaLarge = fatArea;
    			leanAreaLarge = leanArea; 
    			totalAreaLarge = totalAreaCalc;
    			//corrected area = abs(oldArea - sum(residual areas))
    			for(int j=0; j<residuals.size(); j++) {
    				fatArea = Math.abs(fatArea - residuals.get(j).getFatArea(k));
    				partialArea = Math.abs(partialArea - residuals.get(j).getPartialArea(k));
    				leanArea = Math.abs(leanArea - residuals.get(j).getLeanArea(k));
    				totalAreaCalc = Math.abs(totalAreaCalc - residuals.get(j).getTotalAreaCalc(k));
    				totalAreaCount = Math.abs(totalAreaCount - residuals.get(j).getTotalAreaCount(k));
    			}
    			
    			temp.setFatArea(fatArea, k);
    			temp.setPartialArea(partialArea, k);
    			temp.setLeanArea(leanArea, k);
    			temp.setTotalAreaCalc(totalAreaCalc, k);
    			temp.setTotalAreaCount(totalAreaCount, k);
    			
    			meanFatH = getMeanH(v2, FAT_LOWER_BOUND, FAT_UPPER_BOUND);// + OFFSET;
    			meanLeanH = getMeanH(v2, MUSCLE_LOWER_BOUND, MUSCLE_UPPER_BOUND);// + OFFSET;
    		    meanTotalH = getMeanH(v2, FAR_LOWER_BOUND, FAR_UPPER_BOUND);// + OFFSET;
    		    meanFatHResidual = 0;
    		    meanLeanHResidual = 0;
    		    meanTotalHResidual = 0;
    		    
    		    //corrected mean = abs(oldMean*oldArea - sum(residualMean*residualArea))/abs(oldArea - sum(residualArea))
    		    for(int j=0; j<residuals.size(); j++) {
    		    	meanFatHResidual += residuals.get(j).getMeanFatH(k)*residuals.get(j).getFatArea(k);
    		    	meanLeanHResidual += residuals.get(j).getMeanLeanH(k)*residuals.get(j).getLeanArea(k);
    		    	meanTotalHResidual += residuals.get(j).getMeanTotalH(k)*residuals.get(j).getTotalAreaCalc(k);
    		    }
    		    
    		    meanFatH = (meanFatH*fatAreaLarge - meanFatHResidual) / fatArea;
    		    meanLeanH = (meanLeanH*leanAreaLarge - meanLeanHResidual) / leanArea;
    		    meanTotalH = (meanTotalH*totalAreaLarge - meanTotalHResidual) / totalAreaCalc;
    		    
    		    if(meanFatH > 0) {
    		    	meanFatH = -meanFatH;
    		    	meanTotalH = -meanTotalH;
    		    } else if(new Double(meanFatH).equals(Double.NaN)) 
    		    	meanFatH = 0;
    		    if(meanLeanH < 0)
    		    	meanLeanH = -meanLeanH;
    		    else if(new Double(meanLeanH).equals(Double.NaN))
    		    	meanLeanH = 0;
    		    if(new Double(meanTotalH).equals(Double.NaN))
    		    	meanTotalH = 0;
    		    
    		    temp.setMeanFatH(meanFatH, k);
    			temp.setMeanLeanH(meanLeanH, k);
    			temp.setMeanTotalH(meanTotalH, k);
			}
			time = System.currentTimeMillis() - time;
			
			//only place where calculations are generated, setLastCalculated should only appear here
			temp.setLastCalculated(System.currentTimeMillis());
			
			System.out.println("Finished "+currentVOI.getName()+" in "+time);
			done = true;
		}
		
		/**
		 * Whether the runnable has finished.
		 */
		public boolean isFinished() {
			return done;
		}
	
		/**
		 * Gets the total number of pixels that are bounded by lowerBound and upperBound in the VOI.
		 * @return total number of pixels
		 */
		private double getPieceCount(VOI v, int lowerBound, int upperBound) {
			int area = 0;
			BitSet fullMask = new BitSet();
			v.createBinaryMask(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1]);
			double mark = 0;
			for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
		        mark = getImageA().getDouble(i);
				if(mark  >= lowerBound && mark <= upperBound) 
					area++;	
			}
			return area;
		}
		
		/**
		 * Gets MIPAV calculated number of pixels that are in the area of the VOI.
		 */
		private double getTotalAreaCalc(VOI v) {
			return v.area(); //returns volume in 3D
		}
	
		/**
		 * Gets new calculated number of pixels that are in the area of the VOI.
		 */
		private double getTotalAreaCount(VOI v) {
			int totalArea = 0;
			BitSet fullMask = new BitSet();
			v.createBinaryMask(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1]);
			for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) 
		        totalArea++;
			return totalArea;
		}
	
		/**
		 * Gets mean H-unit of all pixels that are bounded by lowerBound and upperBound.
		 */
		private double getMeanH(VOI v, int lowerBound, int upperBound) {
			int area = 0;
			double meanH = 0;
			BitSet fullMask = new BitSet();
			v.createBinaryMask(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1]);
			double mark = 0;
			for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
		        mark = getImageA().getDouble(i);
				if(mark  >= lowerBound && mark <= upperBound) {
					area++;
					meanH += mark;
				}
			}
			meanH /= area;
			if(new Double(meanH).equals(Double.NaN))
				meanH = 0;
			return meanH;
		}
		
		/**
		 * Produces residuals of a given VOI that are used to subtract out irrelevant portions of that VOI
		 */
		
		private ArrayList<PlugInSelectableVOI> getResiduals(VOI v) {
			ArrayList<PlugInSelectableVOI> arr = new ArrayList<PlugInSelectableVOI>();
			if(imageType.equals(ImageType.Abdomen)) {
				if(v.getName().equals("Subcutaneous area")) {
					arr.add(voiBuffer.get("Abdomen"));
				} 
			} else if(imageType.equals(ImageType.Thigh)) {
				if(v.getName().equals("Left Thigh")) {
					arr.add(voiBuffer.get("Left Bone"));
					arr.add(voiBuffer.get("Left Marrow"));
				} else if(v.getName().equals("Right Thigh")) {
					arr.add(voiBuffer.get("Right Bone"));
					arr.add(voiBuffer.get("Right Marrow"));
				} else if(v.getName().equals("Left Bone")) {
					arr.add(voiBuffer.get("Left Marrow"));
				} else if(v.getName().equals("Right Bone")) {
					arr.add(voiBuffer.get("Right Marrow"));
				} 
			}
			return arr;
		}
	
		/**
		 * Makes sure independent VOIs are calculated before their dependencies.  Example
		 * left marrow calculated before left bone by properly ordering them.
		 * @param tempVec
		 * @return
		 */
		private VOIVector reorderVOIs(VOIVector tempVec) {
			VOIVector vec = (VOIVector)tempVec.clone();
			if(imageType.equals(ImageType.Abdomen)) {
				int size = vec.size(), i = 0, count = 0;
				while(count<size) {
					if(vec.get(i).getName().equals("Subcutaneous area")) {
						VOI tempVOI = vec.remove(i);
						vec.add(size-1, tempVOI);
					} else
						i++;
					count++;
				}
			} else if(imageType.equals(ImageType.Thigh)) {
				int size = vec.size(), i = 0, count = 0;
				while(count<size) {
					if(vec.get(i).getName().equals("Left Thigh")) {
						VOI tempVOI = vec.remove(i);
						vec.add(size-1, tempVOI);
					} else if(vec.get(i).getName().equals("Right Thigh")) {
						VOI tempVOI = vec.remove(i);
						vec.add(size-2, tempVOI);
					} else if(vec.get(i).getName().equals("Left Bone")) {
						VOI tempVOI = vec.remove(i);
						vec.add(size-3, tempVOI);
					} else if(vec.get(i).getName().equals("Right Bone")) {
						VOI tempVOI = vec.remove(i);
						vec.add(size-4, tempVOI);
					} else
						i++;
					count++;
				}
			}
			return vec;
		}
	
		/**
		 * Gets the VOI being calculated in this runnable.
		 */
		public VOI getCurrentVOI() {
			return currentVOI;
		}
		
		/**
		 * Sets the VOI being calculated in this runnable to a new value. Also resets name.
		 */
		public void setCurrentVOI(VOI currentVOI) {
			this.currentVOI = currentVOI;
			this.name = currentVOI.getName();
		}
	}
	/**
     * Gets all VOIs of given names from the buffer that have been created
     */
    
    public void getVOIs(String[] voiName, double fillVOIs) {
    	getActiveImage().unregisterAllVOIs();
    	ArrayList<String> newName = new ArrayList<String>();
    	String v;
    	for(int i=0; i<voiName.length; i++) {
    		if(voiBuffer.get(v = voiName[i].substring(0, voiName[i].lastIndexOf('.'))).isCreated())
    			newName.add(v);
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
                    	if(voiBuffer.get(voiVec[0].getName()).getColor().equals(PlugInSelectableVOI.INVALID_COLOR)) {
	                    	Color c = hasColor(voiVec[0]);
	                        if(!(c = hasColor(voiVec[0])).equals(PlugInSelectableVOI.INVALID_COLOR))
	                            voiVec[0].setColor(c);
	                        else 
	                            voiVec[0].setColor(c = colorPick[colorChoice++ % colorPick.length]);
	                        voiBuffer.get(voiVec[0].getName()).setColor(c);
                    	} else
                    		voiVec[0].setColor(voiBuffer.get(voiVec[0].getName()).getColor());
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
    
    /**
     * Whether a particular VOI exists
     * @param name of the VOI to search for
     * @return
     */
    public boolean voiExists(String name) {
    	return voiBuffer.get(name).isCreated();
    }
    
    /**
     * Whether a VOI contains a particular curve for the given slice.  If bad name or slice number, 
     * returns false (performs bounds check)
     * 
     * @param name Name of VOI to check for
     * @param slice Slice number to check
     * @return
     */
    public boolean voiExists(String name, int slice) {
    	if(!multipleSlices)
    		return voiExists(name);
    	if(voiBuffer.get(name).getCurves()[slice].size() > 0)
    		return true;
    	return false;
    }
    
    /**
     * Easy curveExists for checks if curveExists for viewableSlice(). (so no bounds check)
     */
    public boolean curveExists(String name) {
    	if(voiBuffer.get(name).getCurves()[getViewableSlice()].size() > 0) 
    		return true;
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
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
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
    	for(int i=0; i<temp.getZDim(); i++) 
    		temp.removeCurves(i);
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
                MipavUtil.displayError("Invalid VOI from location:\n"+fileDir+"\nWith name: "+name);
            } else {
            	Vector <VOIBase>[] voiVecTemp = voiVec[0].getCurves();
            	for(int i=0; i<voiVecTemp.length; i++) {
        			for(int j=0; j<voiVecTemp[i].size(); j++) { 
        				temp.importCurve((VOIContour)(voiVecTemp[i].get(j)), i);
        			}
        		}
            }
        }
        return temp;
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
	 * Adds the table of voi information to the pdf, adds the images (edge and QA), and closes the document
	 * @param edgeImage
	 * @param qaImage
	 */
	protected void closePDF(java.awt.Image edgeImage, java.awt.Image qaImage) {
			try {
			Paragraph aPar = new Paragraph();
			aPar.setAlignment(Element.ALIGN_CENTER);
			aPar.add(new Paragraph());
			if(multipleSlices)
				aPar.add(new Chunk("Volume calculations", new Font(Font.TIMES_ROMAN, 14, Font.BOLD)));
			else
				aPar.add(new Chunk("Area calculations", new Font(Font.TIMES_ROMAN, 14, Font.BOLD)));
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
			
			MipavUtil.displayInfo("PDF saved to: " + pdfFile+"\nText saved to: "+textFile);
			ViewUserInterface.getReference().getMessageFrame().append("PDF saved to: " + pdfFile + 
										"\nText saved to: "+textFile+"\n", ViewJFrameMessage.DATA);
			
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
		fileDir = getActiveImage().getFileInfo(getViewableSlice()).getFileDirectory()+VOI_DIR;
		fileName = fileDir + File.separator + "NIA_Report.pdf";
		pdfFile = new File(fileName);
		if(pdfFile.exists()) {
			int i=0;
			while(pdfFile.exists() && i<1000) {
				fileName = "NIA_Report-"+(++i)+ ".pdf";
				if(i == 1000) 
					MipavUtil.displayError("Too many PDFs have been created, overwriting "+fileName);
				pdfFile = new File(fileDir + File.separator + fileName);
			}
		}
		System.out.println("Entering try loop.");
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
			
			System.out.println("NOW HERE");

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
			spTable.addCell("Pixel Size: (mm)");
			spTable.addCell(Double.toString(getActiveImage().getResolutions(0)[0]));
			spTable.addCell("Slice Thickness: (mm)");
			spTable.addCell(Float.toString(getActiveImage().getFileInfo()[getViewableSlice()].getSliceThickness()));
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
			System.out.println("CREATED");
			// add Column Titles (in bold)
			String type = new String();
			if(multipleSlices) {
				wholeTable.addCell(new PdfPCell(new Paragraph("Volume (cm^3)", fontBold)));
				type = "Vol";	
			} else {
				wholeTable.addCell(new PdfPCell(new Paragraph("Area (cm^2)", fontBold)));
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
					sliceTable[i].addCell(new PdfPCell(new Paragraph("Area (cm^2)", fontBold)));
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
		    System.out.println("Error occured in addCell's calling method");
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
	protected void addToPDF(String name, double fatArea, double leanArea, double totalAreaCount, 
			double meanFatH, double meanLeanH, double meanTotalH, PdfPTable aTable) {
		
	    try {
			Font fontNormal = FontFactory.getFont("Helvetica", 10, Font.NORMAL, Color.DARK_GRAY);
			if (name.endsWith(".xml")) {
				name = name.substring(0, name.length() - 4);
			}
			if(name == null) {
			    name = new String("Removed");
			    System.out.println("Unexpected null encountered");
			}
			System.out.println("To test: "+name);
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
		    System.out.println("Error adding PDF element.");
		    if(wholeTable == null) 
			System.out.println("aTable");
		    if(name == null) 
			System.out.println("name");
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
		
		/**
		 * Returns the name of the VOI corresponding to this colorButton.
		 */
		public String getVOIName() {
			return voiName;
		}
		
		/**
		 * Sets the color of this colorButton.
		 */
		public void setColor(Color c) {
			colorButton.getColorIcon().setColor(c);
		}
		
		/**
		 * Returns the actual colorButton inside of the panel.
		 */
		public ColorButton getColorButton() {
			return colorButton;
		}
	}
	
	/**
	 * The ColorButton inside of a ColorButtonPanel
	 */
	private class ColorButton extends JButton implements VOIListener {

		/**The icon displaying the current color.*/
		private ColorIcon cIcon;
		
		/**The name of the voi corresponding to this button.*/
		private String voiName;
		
		/**
		 * Constructs a colorButton with the given color and name 
		 * with standard dimension of 20px by 20px.
		 */
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
		
		/**
		 * Returns the colorIcon inside of the colorButton.
		 */
		public ColorIcon getColorIcon() {
			return cIcon;
		}
		
		/**
         * We are not interested in adding curves, so this method is empty.
         */
		public void addedCurve(VOIEvent added) {
            /* not interested in adding curves */
		}
		
        /**
         * We are not interested in removing Curves, so this method is empty.
         */
        public void removedCurve(VOIEvent removed) {
            /* not interested in removing curves */
        }

        /**
         * VOI Listener call (listens only to color changes)
         */
        public void colorChanged(Color c) {
        	cIcon.setColor(c);
        	//voiBuffer.get(voiName).setColor(c);
        	this.repaint();
        }
        
        /**
         * We are not interested in selecting Curves, so this method is empty.
         */
        public void selectedVOI(VOIEvent selection) {
        	/* not interested in having the ColorButon select VOIs */
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
			//TODO: Implement new handler here
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
	                		if(imageActive.getVOIs().get(i).area() > 0)
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
	     * @param  voiList  DOCUMENT ME!
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

	        everything = new JTabbedPane(JTabbedPane.TOP);
	        everything.setFont(MipavUtil.font12B);
	        everything.insertTab("VOI selection", null, buildVOIPanel(voiList), // we must store this panel so we can
	                                                                            // create a new listing later
	                             "Choose VOIs and statistics file", VOI_TAB);

	        JPanel statPanel = new JPanel(new BorderLayout());
	        checkBoxPanel = new PlugInStatisticsList();
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
	                contours = ((VOI) list.getElementAt(i)).getCurves();

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
	    	                    	if(v instanceof PlugInSelectableVOI && ((PlugInSelectableVOI)v).calcEligible()) {
	    	                    		if(allNames[k].equals(TOTAL_AREA)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getTotalAreaCalc(slice));
	                        				logTotalData[count] = dec.format(((PlugInSelectableVOI)v).getTotalAreaCalc(slice));
	    	                    		} else if(allNames[k].equals(FAT_AREA)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getFatArea(slice));
	                        				logTotalData[count] = dec.format(((PlugInSelectableVOI)v).getFatArea(slice));
	    	                    		} else if(allNames[k].equals(LEAN_AREA)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getLeanArea(slice));
	                        				logTotalData[count] = dec.format(((PlugInSelectableVOI)v).getLeanArea(slice));
	    	                    		} else if(allNames[k].equals(MEAN_TOTAL_HU)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanTotalH(slice));
	                        				logTotalData[count] = dec.format(((PlugInSelectableVOI)v).getMeanTotalH(slice));
	    	                    		} else if(allNames[k].equals(MEAN_FAT_HU)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanFatH(slice));
	                        				logTotalData[count] = dec.format(((PlugInSelectableVOI)v).getMeanFatH(slice));
	    	                    		} else if(allNames[k].equals(MEAN_LEAN_HU)) {
	                        				rowData[count] = dec.format(((PlugInSelectableVOI)v).getMeanLeanH(slice));
	                        				logTotalData[count] = dec.format(((PlugInSelectableVOI)v).getMeanLeanH(slice));
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
	                contours = ((VOI) list.getElementAt(i)).getCurves();

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
	                    	if(v instanceof PlugInSelectableVOI && ((PlugInSelectableVOI)v).calcEligible()) {
	                    		if(allNames[k].equals(TOTAL_AREA)) {
                    				rowData[count] = dec.format(((PlugInSelectableVOI)v).getTotalAreaCalc());
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
	    		System.out.println("Orginial fill: "+i);
	        	labels[i] = statisticDescription[i];
	        }
	    	
	    	for(int i=statisticDescription.length; i<labels.length; i++) {
	    		System.out.println("Next step: "+i);
	    		labels[i] = calcExtra[i-statisticDescription.length];
	    	}
	    	
	    	return labels;
	    }
	    
	    protected void setListLength() {
	        listLength = numberOfStatistics+calcExtra.length;
	    }
			
	}

	 //TODO: Work with right mouse click
	
	private class PlugInVOIStats extends JDialogVOIStats {
		
	
	    public PlugInVOIStats(Frame theParentFrame, ModelImage img, VOI _voi) {
	        super(theParentFrame, img, _voi);
	    }
	    

	    protected void init() {

	        // setTitle("VOI Statistics");
	        frameBorder = BorderFactory.createCompoundBorder(BorderFactory.createRaisedBevelBorder(),
	                                                         BorderFactory.createLoweredBevelBorder());

	        JLabel labelName = new JLabel("Name of VOI:");
	        labelName.setFont(serif12);
	        labelName.setForeground(Color.black);

	        JLabel labelColor = new JLabel("Color of VOI:");
	        labelColor.setFont(serif12);
	        labelColor.setForeground(Color.black);

	        JLabel labelThickness = new JLabel("Thickness of VOI:");
	        labelThickness.setFont(serif12);
	        labelThickness.setForeground(Color.black);

	        colorButton = new JButton();
	        colorButton.setPreferredSize(new Dimension(25, 25));
	        colorButton.setToolTipText("Change VOI color");
	        colorButton.addActionListener(this);

	        VOIName = new JTextField(15);
	        VOIName.setFont(serif12);

	        VOIThicknessField = new JTextField(3);
	        VOIThicknessField.setFont(serif12);
	        MipavUtil.makeNumericsOnly(VOIThicknessField, false);

	        JPanel namePanel = new JPanel(new GridBagLayout());
	        GridBagConstraints gbc = new GridBagConstraints();
	        gbc.insets = new Insets(5, 5, 5, 5);
	        gbc.anchor = GridBagConstraints.WEST;
	        gbc.weightx = 0;
	        gbc.weighty = 0;
	        gbc.fill = GridBagConstraints.NONE;
	        namePanel.add(labelName, gbc);

	        gbc.weightx = 1;
	        gbc.weighty = 1;
	        gbc.gridx = 1;
	        gbc.fill = GridBagConstraints.HORIZONTAL;
	        namePanel.add(VOIName, gbc);

	        gbc.weightx = 0;
	        gbc.weighty = 0;
	        gbc.gridx = 0;
	        gbc.gridy = 1;
	        gbc.fill = GridBagConstraints.NONE;
	        namePanel.add(labelThickness, gbc);

	        gbc.weightx = 1;
	        gbc.weighty = 1;
	        gbc.gridx = 1;
	        gbc.fill = GridBagConstraints.HORIZONTAL;
	        namePanel.add(VOIThicknessField, gbc);

	        gbc.gridx = 0;
	        gbc.gridy = 2;
	        gbc.weightx = 0;
	        gbc.weighty = 0;
	        gbc.fill = GridBagConstraints.NONE;
	        namePanel.add(labelColor, gbc);

	        gbc.gridx = 1;
	        gbc.weightx = 1;
	        gbc.weighty = 1;
	        namePanel.add(colorButton, gbc);

	        gbc.insets = new Insets(0, 0, 0, 0);

	        checkboxBoundingBox = new JCheckBox("Show contour bounding box");
	        checkboxBoundingBox.setFont(serif12);

	        checkboxAdditiveOrSubtractive = new JCheckBox("Use additive polarity for VOI");
	        checkboxAdditiveOrSubtractive.setFont(serif12);

	        checkboxIncludeForProcessing = new JCheckBox("Include for processing");
	        checkboxIncludeForProcessing.setFont(serif12);

	        checkboxBoundary = new JCheckBox("Display VOI shading");
	        checkboxBoundary.setFont(serif12);
	        checkboxBoundary.addItemListener(this);

	        checkboxVOIName = new JCheckBox("Show VOI name");
	        checkboxVOIName.setFont(serif12);
	        checkboxVOIName.setSelected(Preferences.is(Preferences.PREF_SHOW_VOI_NAME));

	        JPanel checkboxPanel = new JPanel();
	        checkboxPanel.setLayout(new BoxLayout(checkboxPanel, BoxLayout.Y_AXIS));
	        checkboxPanel.add(checkboxBoundingBox);
	        checkboxPanel.add(checkboxAdditiveOrSubtractive);
	        checkboxPanel.add(checkboxIncludeForProcessing);
	        checkboxPanel.add(checkboxVOIName);
	        checkboxPanel.add(checkboxBoundary);

	        opacitySlider = new JSlider(SwingConstants.HORIZONTAL, 0, 100, 30);

	        opacitySlider.setMajorTickSpacing(20);
	        opacitySlider.setValue(30);
	        opacitySlider.setPaintTicks(true);
	        opacitySlider.setEnabled(false);
	        opacitySlider.addChangeListener(this);

	        JLabel maximum = new JLabel(String.valueOf(1));
	        maximum.setForeground(Color.black);
	        maximum.setFont(serif12);

	        current = new JLabel(String.valueOf(opacitySlider.getValue() / 100.0f));
	        current.setForeground(Color.black);
	        current.setFont(serif12B);

	        JLabel minimum = new JLabel(String.valueOf(0));
	        minimum.setForeground(Color.black);
	        minimum.setFont(serif12);

	        JPanel sliderPanel = new JPanel(new GridBagLayout());

	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.gridwidth = 3;
	        gbc.weightx = 1;
	        gbc.gridheight = 1;
	        gbc.fill = GridBagConstraints.HORIZONTAL;

	        sliderPanel.add(opacitySlider, gbc);

	        gbc.gridx = 0;
	        gbc.gridy = 1;
	        gbc.gridwidth = 1;
	        gbc.weightx = 0;
	        gbc.anchor = GridBagConstraints.WEST;
	        gbc.fill = GridBagConstraints.NONE;

	        sliderPanel.add(minimum, gbc);

	        gbc.gridx = 1;
	        gbc.anchor = GridBagConstraints.CENTER;
	        gbc.weightx = .5;

	        sliderPanel.add(current, gbc);

	        gbc.gridx = 2;
	        gbc.anchor = GridBagConstraints.EAST;
	        gbc.weightx = 0;

	        sliderPanel.add(maximum, gbc);
	        sliderPanel.setBorder(buildTitledBorder("Opacity"));

	        JPanel panelVOIProps = new JPanel(new GridBagLayout());
	        panelVOIProps.setBorder(buildTitledBorder("VOI properties"));
	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.gridwidth = 1;
	        gbc.gridheight = 1;
	        gbc.weightx = 1;
	        gbc.anchor = GridBagConstraints.WEST;
	        gbc.fill = GridBagConstraints.HORIZONTAL;
	        panelVOIProps.add(namePanel, gbc);
	        gbc.gridy = 1;
	        panelVOIProps.add(checkboxPanel, gbc);
	        gbc.gridy = 2;
	        gbc.weighty = 1;
	        gbc.anchor = GridBagConstraints.NORTH; // gbc.fill = GridBagConstraints.BOTH;
	        panelVOIProps.add(sliderPanel, gbc);

	        listPanel = new PlugInStatisticsList();

	        try {
	            listPanel.setSliceCount(image.getExtents()[2]);
	        } catch (ArrayIndexOutOfBoundsException aioobe) {
	            // otherwise, this must be a 2d image.
	            listPanel.setSliceCount(1);
	        } finally {
	            listPanel.setCheckBoxesEnabled();
	        }

	        checkboxExclude = new JCheckBox("Exclude intensity range");
	        checkboxExclude.setFont(serif12);
	        checkboxExclude.addActionListener(this);
	        
	        labelMin = new JLabel("Range: ");
	        labelMin.setFont(serif12);
	        labelMin.setForeground(Color.black);
	        labelMin.setEnabled(false);

	        textMin = new JTextField(5);
	        textMin.setEnabled(false);
	        textMin.setFont(serif12);

	        labelMax = new JLabel(" to ");
	        labelMax.setFont(serif12);
	        labelMax.setForeground(Color.black);
	        labelMax.setEnabled(false);

	        textMax = new JTextField(5);
	        textMax.setEnabled(false);
	        textMax.setFont(serif12);
	        
	        checkboxSaveStats = new JCheckBox("Save statistics in header");
	        checkboxSaveStats.setFont(serif12);
	        //checkboxSaveStats.addActionListener(this);

	        JPanel checkPanel = new JPanel(new GridBagLayout());
	        GridBagConstraints gbc2 = new GridBagConstraints();
	        gbc2.anchor = GridBagConstraints.WEST;
	        gbc2.fill = GridBagConstraints.BOTH;
	        gbc2.weightx = 1;
	        gbc2.weighty = 1;
	        gbc2.gridx = 0;
	        gbc2.gridy = 0;
	        checkPanel.add(checkboxExclude, gbc2);
	        gbc2.gridy++;
	        checkPanel.add(checkboxSaveStats, gbc2);

	        JPanel rangePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

	        rangePanel.add(labelMin);
	        rangePanel.add(textMin);
	        rangePanel.add(labelMax);
	        rangePanel.add(textMax);

	        JPanel intensityPanel = new JPanel();
	        intensityPanel.setLayout(new BoxLayout(intensityPanel, BoxLayout.Y_AXIS));
	        intensityPanel.add(checkPanel);
	        intensityPanel.add(rangePanel);

	        statsPanel = new JPanel(new BorderLayout());
	        statsPanel.add(listPanel);
	        statsPanel.add(intensityPanel, BorderLayout.SOUTH);

	        JLabel labelSeed = new JLabel("Seed value (0-32K)");
	        labelSeed.setFont(serif12);
	        labelSeed.setForeground(Color.black);

	        seedValueTF = new JTextField(5);
	        seedValueTF.setFont(serif12);
	        seedValueTF.addFocusListener(this);

	        JPanel seedValuePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
	        seedValuePanel.setBorder(buildTitledBorder("Watershed seed value"));
	        seedValuePanel.add(labelSeed);
	        seedValuePanel.add(seedValueTF);

	        JPanel calcPanel = new JPanel(new BorderLayout());
	        calcPanel.add(statsPanel);
	        calcPanel.add(seedValuePanel, BorderLayout.SOUTH);

	        applyButton = new JButton("Apply");
	        applyButton.setPreferredSize(MipavUtil.defaultButtonSize);
	        applyButton.setFont(serif12B);
	        applyButton.addActionListener(this);

	        cancelButton = buildCancelButton();
	        cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
	        
	        helpButton = buildHelpButton();
	        helpButton.setPreferredSize(MipavUtil.defaultButtonSize);

	        // build the VOI tree
	        buildVOITree();
	        buildVOIContourPane();

	        GridBagConstraints gb = new GridBagConstraints();

	        JPanel mainTreePanel = new JPanel(new GridBagLayout());
	        mainTreePanel.setBorder(buildTitledBorder("VOI Browser"));

	        gb.anchor = GridBagConstraints.CENTER;
	        gb.gridx = 0;
	        gbc.gridy = 0;
	        gb.weightx = 1.0;
	        gb.weighty = 1.0;
	        gb.fill = GridBagConstraints.BOTH;

	        mainTreePanel.add(voiTreePane, gb);

	        JPanel treeOptionPanel = new JPanel(new BorderLayout());
	        treeOptionPanel.setBorder(buildTitledBorder("Tree options"));
	        followVOISelectionBox = new JCheckBox("Frame follows VOI selection", true);
	        followVOISelectionBox.setFont(MipavUtil.font12);
	        followVOISelectionBox.addActionListener(this);
	        followVOISelectionBox.setEnabled(image.getNDims() > 2);
	        treeOptionPanel.add(followVOISelectionBox, BorderLayout.CENTER);


	        gb.gridy = 1;
	        gb.weightx = 1;
	        gb.weighty = 0;
	        gb.fill = GridBagConstraints.HORIZONTAL;
	        mainTreePanel.add(treeOptionPanel, gb);

	        gb.gridy = 2;
	        gb.weightx = .5;
	        gb.weighty = .5;
	        gb.fill = GridBagConstraints.BOTH;
	        mainTreePanel.add(voiContourPane, gb);

	        JPanel leftButton = new JPanel();
	        leftButton.add(applyButton);
	        leftButton.add(cancelButton);
	        leftButton.add(helpButton);

	        JPanel leftWholePanel = new JPanel(new BorderLayout());
	        leftWholePanel.add(panelVOIProps, BorderLayout.NORTH);
	        leftWholePanel.add(mainTreePanel, BorderLayout.CENTER);

	        JPanel leftPanel = new JPanel(new BorderLayout());
	        leftPanel.add(leftWholePanel);
	        leftPanel.add(leftButton, BorderLayout.SOUTH);

	        calcButton = new JButton("Calculate");
	        calcButton.setPreferredSize(new Dimension(100, 30));
	        calcButton.setFont(serif12B);
	        calcButton.addActionListener(this);

	        JPanel rightButton = new JPanel();
	        rightButton.add(calcButton);
	        //rightButton.add(helpButton);

	        JPanel rightPanel = new JPanel(new BorderLayout());
	        rightPanel.add(calcPanel);
	        rightPanel.add(rightButton, BorderLayout.SOUTH);

	        mainDialogPanel.setLayout(new GridBagLayout());
	        gb.gridx = 0;
	        gb.gridy = 0;
	        gb.weightx = 1;
	        gb.weighty = 1;
	        gb.fill = GridBagConstraints.BOTH;
	        mainDialogPanel.add(leftPanel, gb);

	        gb.gridx = 1;
	        mainDialogPanel.add(rightPanel, gb);


	        // mainDialogPanel.setLayout(new BorderLayout());
	        // mainDialogPanel.add(leftPanel, BorderLayout.WEST);
	        // mainDialogPanel.add(rightPanel);
	        mainDialogPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
	        getContentPane().add(mainDialogPanel);
	        pack();

	    }
	}
	
	private class PlugInHandler extends VOIHandler {
		
		public PlugInHandler(ViewJComponentEditImage compImage) {
			super(compImage);
			
			voiDialog = new PlugInVOIStats(compImage.getFrame(), compImage.getActiveImage(), null);
		}
	}
	
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
	    	
	    	if(file.exists() == true) 
                file.delete();
	    	
	    	//now that file has been deleted, prompting will not occur
	    	super.writeXML(voi, saveAllContours);
	    }
	}
	
}
