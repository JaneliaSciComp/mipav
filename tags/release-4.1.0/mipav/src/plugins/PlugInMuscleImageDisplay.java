import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmSnake;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.ScriptRecorder;
import gov.nih.mipav.model.scripting.actions.ActionCloseFrame;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.event.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;
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

public class PlugInMuscleImageDisplay extends ViewJFrameImage implements KeyListener, AlgorithmInterface {
    
    //~ Static fields --------------------------------------------------------------------------------------------------
    
    public static final Color[] colorPick = {Color.GREEN, Color.ORANGE, Color.CYAN, 
                                                Color.YELLOW, Color.MAGENTA, Color.RED};
    
    //~ Instance fields ------------------------------------------------------------------------------------------------    
    

    /**For writing PDF docs below. */
    private Document pdfDocument = null;
	private PdfWriter pdfWriter = null;
	private File pdfFile = null;
	private File textFile = null;
	private PdfPTable aTable = null;
	private PdfPTable imageTable = null;
    
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
    
    private JTabbedPane dialogTabs;
    
    private int activeTab;
    
    private DialogPrompt[] tabs;
    
    private int currentSlice = 0;
    
    /**Whether the algorithm is dealing with a 3D CT image. */
    private boolean multipleSlices;
    
    private String[] titles; 
    
    private String imageDir;
    
    private boolean voiChangeState = false;

    private int colorChoice = 0;
    
    private long time = 0;
    
    /**Buffer containing exact copies of VOIs on file system along with program relevant material.*/
    private Map<String, PlugInSelectableVOI> voiBuffer;
    
    private ThreadGroup calcGroup = new ThreadGroup("CalcVOI");
    
    private PlugInAlgorithmCTBone boneSeg;
    
    private PlugInAlgorithmCTMarrow marrowSeg;
    
    private PlugInAlgorithmCTThigh thighSeg;
    
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
    	ViewJProgressBar progressBar = new ViewJProgressBar("Automatic Seg", "Initializing...", 0, 100, true);
    	setVisible(false);
        
        Preferences.setProperty(Preferences.PREF_CLOSE_FRAME_CHECK, String.valueOf(true));
        
        this.setImageA(image);
        this.setActiveImage(IMAGE_A);
        this.titles = titles;
        this.mirrorArr = new String[voiList.length][];
        this.mirrorZ = new boolean[voiList.length][];
        this.noMirrorArr = new String[voiList.length][];
        this.noMirrorZ = new boolean[voiList.length][];
        this.calcTree = new TreeMap();
        this.voiBuffer = Collections.synchronizedMap(new TreeMap<String, PlugInSelectableVOI>());
        this.imageType = imageType;
        this.symmetry = symmetry;
        this.multipleSlices = multipleSlices;
        this.currentSlice = getViewableSlice();
        
        //already added from super constructor
        //image.addImageDisplayListener(this);
        
        for(int i=0; i<voiList.length; i++) {
        	ArrayList mirrorArrList = new ArrayList(), noMirrorArrList = new ArrayList(), 
        				mirrorZList = new ArrayList(), noMirrorZList = new ArrayList();
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
        progressBar.updateValue(5);
        imageDir = getImageA().getFileInfo(getViewableSlice()).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
        
        File f;
        if(!(f = new File(imageDir+"\\")).exists())
        	f.mkdir();
        
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
        
        Iterator itr = voiBuffer.keySet().iterator();
        
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
        this.voiBuffer = new TreeMap();
        
        createVOIBuffer();
        
        //create automatic VOIs here
        
        for(int i=0; i<voiList.length; i++) {
        	ArrayList mirrorArrList = new ArrayList(), noMirrorArrList = new ArrayList(), 
        				mirrorZList = new ArrayList(), noMirrorZList = new ArrayList();
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
    	
    	ctMode(getImageA(), -175, 275);
    	
    	imageDir = getImageA().getFileInfo(getViewableSlice()).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR;
    	
    	
    	
    	
    }
    
    private void waitAlg(ViewJProgressBar progressBar) {
    	long time = System.currentTimeMillis();
    	int extend = 0;
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
    }
    
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
		    	//while(!boneSeg.reachedCheckPoint()) {
		    	//    
		    	//}
		    	//if(boneSeg.reachedCheckPoint() && boneSeg.boneOK()) {   	
		    	//    marrowSeg.setCM(boneSeg.getX1CMs(), boneSeg.getX2CMs(), boneSeg.getY1CMs(), boneSeg.getY2CMs());
		    	    performSegmentation(marrowSeg, resultImage2);
		    	    
		    	//}
    		}
	    	
    		if(voiBuffer.get("Left Thigh").area() == 0 && voiBuffer.get("Right Thigh").area() == 0) {
		    	ModelImage resultImage3 = (ModelImage)srcImage.clone();
		    	thighSeg = new PlugInAlgorithmCTThigh(resultImage3, srcImage, imageDir, voiBuffer.get("Left Thigh").getColor());
		    	performSegmentation(thighSeg, resultImage3);
    		}
    	}
    }
    
    private boolean performSegmentation(AlgorithmBase genericAlgo, ModelImage resultImage) {
    	try {
            String name = JDialogBase.makeImageName(getActiveImage().getImageName(), "_kidneys");
            resultImage.setImageName(name);
            
            //genericAlgo = new PlugInAlgorithmNewGeneric2(resultImage, image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            genericAlgo.addListener(this);
            //createProgressBar(srcImage.getImageName(), " ...", genericAlgo);

            //setVisible(false); // Hide dialog

            //if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (genericAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    System.err.println("A thread is already running on this object");
                    return false;
                }
            //} else {
           //     genericAlgo.run();
            //}
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
		}

        if (algorithm != null) {
            algorithm.finalize();
            algorithm = null;
        }
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
        String command = e.getActionCommand();
        //run through toggle buttons to see if a menu selected one (updates the button status)
        getControls().getTools().setToggleButtonSelected(command);
                
        if(command.equals(DialogPrompt.CHECK_VOI)) {
        	String name;
            ((VoiDialogPrompt)tabs[voiTabLoc]).setUpDialog(name = ((JButton)(e.getSource())).getText(), 
            		true, voiBuffer.get(name).getMaxCurvesPerSlice());
            lockToPanel(voiTabLoc, "VOI"); //includes making visible
            //TODO: add here
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
        	((AnalysisPrompt)tabs[resultTabLoc]).setButtons();

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
        
    	time = System.currentTimeMillis();
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
        // componentImage.deactivateAllVOI();
        
        if(currentSlice != slice) {
        	System.out.println("Changed Slice!");
        	currentSlice = slice;
        }
        
        componentImage.getVOIHandler().resetLivewire();
        setTitle();
        	//if(changeSlice && activeTab < voiTabLoc) {
        	//if(tabs[resultTabLoc].isVisible()) 
        	//((AnalysisPrompt)tabs[resultTabLoc]).setButtons();
			//((MuscleDialogPrompt)tabs[activeTab]).clearButtons();
        
    	//if(slice != getViewableSlice())
    	//	changeSlice = true;
    	//super.setSlice(slice, updateLinkedImages);

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
    
    private void initMuscleButton(int pane) {
    	VOIVector vec = getActiveImage().getVOIs();
    	for(int i=0; i<vec.size(); i++) { 
        	PlugInSelectableVOI temp = null;
        	if((temp = voiBuffer.get(vec.get(i).getName())) != null && temp.getCurves()[getViewableSlice()].size() > 0) {
        		((MuscleDialogPrompt)tabs[pane]).setButton(temp, temp.getName(), temp.getColor());
        		if(temp.isComputerGenerated()) {
        			((MuscleDialogPrompt)tabs[pane]).setButtonAutomatic(temp.getName());
        		}
        	}
        	
        } 
    }
    
    private void initMuscleImage(int pane) {        
    	
    	componentImage.setCursorMode(ViewJComponentBase.NEW_VOI);
    	getVOIs(pane);
        
        System.out.println("Active tab: :"+activeTab);
    	initMuscleButton(pane);
        
    	this.repaint();

        updateImages(true);
    }
    
    private void createVOIBuffer() {
    	Iterator voiItr = voiBuffer.keySet().iterator();
    	
    	while(voiItr.hasNext()) {
    		PlugInSelectableVOI v = loadVOI((String)voiItr.next());
    		
    		Color c = PlugInSelectableVOI.INVALID_COLOR;
			
			if((c = v.getColor()).equals(PlugInSelectableVOI.INVALID_COLOR)) {
            	if((c = hasColor(v)).equals(PlugInSelectableVOI.INVALID_COLOR))
                    v.setColor(c = colorPick[colorChoice++ % colorPick.length]);
            	else
            		v.setColor(c);
			} else
				v.setColor(c);
			System.out.println(v.getName() +" "+ v.getColor());
    	}
    }
    
    private void getVOIs(int pane) {
    	
    	Iterator voiItr = voiBuffer.keySet().iterator();
    	PlugInSelectableVOI v;
    	while(voiItr.hasNext()) {
    		String name = (String)voiItr.next();
    	
	    	if(voiBuffer.get(name).getLocation() == pane) {
	    		v = getSingleVOI(name);
	    		v.setThickness(2);
	    		v.setDisplayMode(VOI.BOUNDARY);
    			getActiveImage().registerVOI(v);
	    	}
    	}
    }
    
    private void initVoiImage() {
    	//VOIs of pane already loaded, just need to make relevant ones solid
        VOIVector voiVec = getActiveImage().getVOIs();
    	for(int i=0; i<voiVec.size(); i++) {
    		VOI voi = voiVec.get(i);
    		if(voiBuffer.get(voi.getName()).fillEligible() && 
    				!(((VoiDialogPrompt)tabs[voiTabLoc]).getObjectName().equals(voi.getName()))) 
    			voi.setDisplayMode(VOI.SOLID);
    	}
        
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
    	
    	protected JButton buttonGroup[];
    	
    	protected PlugInMuscleImageDisplay muscleFrame;
    	
    	private Vector<ActionListener> objectList = new Vector<ActionListener>();
    	
    	protected String title;
    	
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

    private class VoiDialogPrompt extends DialogPrompt implements ActionListener, AlgorithmInterface {

        private final String[] buttonStringList = {OK, HIDE_ALL, CANCEL, RESET, HIDE_ONE};
    	
        private String objectName;
        
        private boolean closedVoi;
        
        private int numVoi;
        
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

        private JMenu propMenu;
        
        public VoiDialogPrompt(PlugInMuscleImageDisplay theParentFrame) {
            super(theParentFrame, "VOI");
           
            setButtons(buttonStringList);
            
            voiPromptBuffer = new VOIVector();
            
            initDialog();
        }
        
        private boolean voiExists(String objectName) {     	
            String fileDir;
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
                        String dir = getImageA().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"\\";
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
                } else if (command.equals(HELP)) {
                    PlugInMuscleSegmentation.showHelp("MS00001");
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
                	//TODO: Perform propogate  + smooth on one VOI here
                    //if (componentImage.getVOIHandler().propVOI(1, false) == true) {
                    //    incSlice();
                    //}
                	performSnake(AlgorithmSnake.PROP_NEXT);
                } else if (command.equals("PropVOIDown")) {
                	//TODO: Perform propogate  + smooth on one VOI here
                    //if (componentImage.getVOIHandler().propVOI(-1, false) == true) {
                    //    decSlice();
                    //}
                	performSnake(AlgorithmSnake.PROP_PREV);
                } else if (command.equals("PropVOIAll")) {
                	//TODO: Perform propogate  + smooth on all VOIs here
                    //componentImage.getVOIHandler().propVOIAll();
                	performSnake(AlgorithmSnake.PROP_ALL);
                } 
            }
            
        public void algorithmPerformed(AlgorithmBase algorithm) {
			// TODO Auto-generated method stub
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
        
        
        public void takeDownDialog() {
        	
        	removeAll();
        	voiPromptBuffer.removeAllElements();
        }
        
        public void setUpDialog(String name, boolean closedVoi, int numVoi) {	
        	this.objectName = name;
        	this.closedVoi = closedVoi;
        	this.numVoi = numVoi;
        	warningText.setText("");
        	
        	voiPromptBuffer.removeAllElements();
        	
        	voiExists = voiExists(objectName);
        	
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
            int countQualifiedVOIs = 0; //equal to numVoi when the right  amount of VOIs have been created
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
            
            if(goodVOI != null) {
            	Vector<VOIBase>[] curves = goodVOI.getCurves();
            	for(int i=0; i<curves.length; i++) { 
            		for(int j=0; j<curves[i].size(); j++) {
            			if(curveReplace) 
            				voiBuffer.get(objectName).removeCurve(j, i);
            		    voiBuffer.get(objectName).importCurve((VOIContour)curves[i].get(j), i);
            		}
            	}
            }
            
            return voiBuffer.get(objectName);
           
            /*Vector[] curves = goodVOI.getCurves();
            VOI voi = goodVOI;
            if(curves[getViewableSlice()].size() <= numVoi) {
                for(int i=0; i<numVoi; i++) {
                    if(closedVoi) {
                        goodVOI.setName(objectName);
                        return goodVOI;
                    } else {
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
            
            return null;*/
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
        		mirrorCheckArr[i].setColor(Color.pink);
        		mirrorCheckArr[i].getColorButton().colorChanged(Color.PINK);
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
        
        public void setButton( VOI v, String buttonName, Color c) {
        	if(!c.equals(Color.BLACK)) 
        		voiBuffer.get(buttonName).setCreated(true);
        	for(int i=0; i<mirrorButtonArr.length; i++) {
        		if(buttonName.equals(mirrorButtonArr[i].getText())) {
        			mirrorCheckArr[i].setColor(c);
        			v.addVOIListener(mirrorCheckArr[i].getColorButton());
        			mirrorButtonArr[i].setForeground(Color.BLACK);
        		}
        	}
        	for(int i=0; i<noMirrorButtonArr.length; i++) {
        		if(buttonName.equals(noMirrorButtonArr[i].getText())) {
        			noMirrorCheckArr[i].setColor(c);
        			v.addVOIListener(noMirrorCheckArr[i].getColorButton());
        			noMirrorButtonArr[i].setForeground(Color.BLACK);
        		}
        	}
        }
        
        public void setButtonAutomatic(String buttonName) {
        	for(int i=0; i<mirrorButtonArr.length; i++) {
        		if(buttonName.equals(mirrorButtonArr[i].getText())) {
        			mirrorButtonArr[i].setForeground(Color.RED);
        		}
        	}
        	for(int i=0; i<noMirrorButtonArr.length; i++) {
        		if(buttonName.equals(noMirrorButtonArr[i].getText())) {
        			noMirrorButtonArr[i].setForeground(Color.RED);
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
		
		//private MuscleCalculation muscleCalc = new MuscleCalculation();
		
		private CustomOutput ucsdOutput = new CustomOutput();

		private boolean lutOn = false;
		
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
					//Stays black until pressed.
					checkBoxLocationTree.get(it.next()).setColor(Color.BLACK);
				}
			}
			boolean voiExists = false;
			for(int index=0; index<mirrorButtonArr.length; index++) {
				for(int i=0; i<mirrorButtonArr[index].length; i++) {
					mirrorButtonArr[index][i].setEnabled(voiExists = voiExists(mirrorButtonArr[index][i].getText(), getViewableSlice()));
					mirrorButtonArr[index][i].setForeground(Color.BLACK);
					if(voiExists && voiBuffer.get(mirrorButtonArr[index][i].getText()).isComputerGenerated())
						mirrorButtonArr[index][i].setForeground(Color.RED);
				}
				for(int i=0; i<noMirrorButtonArr[index].length; i++) {
					noMirrorButtonArr[index][i].setEnabled(voiExists(noMirrorButtonArr[index][i].getText(), getViewableSlice()));
					noMirrorButtonArr[index][i].setForeground(Color.BLACK);
					if(voiExists && voiBuffer.get(noMirrorButtonArr[index][i].getText()).isComputerGenerated())
						noMirrorButtonArr[index][i].setForeground(Color.RED);
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
			Iterator itr;
			if(all)
				itr = voiBuffer.keySet().iterator();
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
					    		
				Thread output = new Thread(ucsdOutput);
		    	output.start();
				
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
	    
		private class CustomOutput implements Runnable {
			
			private boolean done = false;
			
			public void run() {
				long time = System.currentTimeMillis();
				ArrayList<PlugInSelectableVOI> calcList = new ArrayList();
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
					String id = ((String)fileInfo.getTagTable().getValue("0010,0020")).trim();
					sliceStr[i] += (id.length() > 0 ? id : "0")+"\t";
					//slice number
					sliceStr[i] += Integer.toString(i)+"\t";
					//scan date
					String dateStr = ((String)fileInfo.getTagTable().getValue("0008,0020")).trim();
					sliceStr[i] += (dateStr.length() > 0 ? dateStr : "0")+"\t";
					//center
					String center = ((String)fileInfo.getTagTable().getValue("0008,0080")).trim();
					sliceStr[i] += (center.length() > 0 ? center : "0")+"\t";
					//analysis date
					sliceStr[i] += dateFormat.format(date)+"\t";
					//analyst
					sliceStr[i] += System.getProperty("user.name")+"\t";
					//pixel size
					sliceStr[i] += dec.format(getActiveImage().getResolutions(0)[0]*.1)+"\t";
					//slice thickness (mm)
					sliceStr[i] += dec.format(getActiveImage().getFileInfo()[getViewableSlice()].getSliceThickness())+"\t";
					//table height (cm)
					String height = dec.format(Double.valueOf((String)fileInfo.getTagTable().getValue("0018,1130")));
					sliceStr[i] += (height.length() > 0 ? height : "0")+"\t";
					
					//insertCalculations
					PlugInSelectableVOI temp = null;
					ArrayList<PlugInSelectableVOI> calcItems = new ArrayList(voiBuffer.keySet().size());
					Iterator<PlugInSelectableVOI> firstItr = voiBuffer.values().iterator();
					while(firstItr.hasNext()) {
						if((temp = firstItr.next()).calcEligible())
							calcItems.add(temp);
					}
					ArrayList<PlugInSelectableVOI> orderedCalcItems = (ArrayList)calcItems.clone();
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
		public static final int OFFSET = 1024;
		public static final int FAT_LOWER_BOUND = -190;
		public static final int FAT_UPPER_BOUND = -30;
		public static final int MUSCLE_LOWER_BOUND = 0;
		public static final int MUSCLE_UPPER_BOUND = 100;
		public static final int FAR_LOWER_BOUND = -2048;
		public static final int FAR_UPPER_BOUND = 2048;
		
		private VOI currentVOI;
		
		private boolean done = false;
		
		private String name;
		
		public MuscleCalculation(VOI newVOI, String name) {
			super();
			this.currentVOI = newVOI;
			this.name = name;
		}
		
		public void run() {
			done = false;
			//ViewJProgressBar progressBar = new ViewJProgressBar("Calculations", "Initializing...", 0, 100, true);
			long time = System.currentTimeMillis();
			
			ArrayList<PlugInSelectableVOI> residuals = new ArrayList();
			VOI v = currentVOI;
			double multiplier = 0.0;
			multiplier = Math.pow(getActiveImage().getResolutions(0)[0]*.1, 2);
			if(multipleSlices)
				multiplier *= getActiveImage().getResolutions(0)[2];
			System.out.println("Multiplier: "+multiplier);
			PlugInSelectableVOI temp = voiBuffer.get(name);
			residuals = getResiduals(temp);
			ArrayList<Thread> calc = new ArrayList(residuals.size());
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
			double fatArea = getPieceCount(v, FAT_LOWER_BOUND, FAT_UPPER_BOUND)*multiplier;
			double partialArea = getPieceCount(v, FAT_UPPER_BOUND, MUSCLE_LOWER_BOUND)*multiplier; 
			double leanArea = getPieceCount(v, MUSCLE_LOWER_BOUND, MUSCLE_UPPER_BOUND)*multiplier; 
			double totalAreaCalc = getTotalAreaCalc(v)*multiplier;
			double totalAreaCount = getTotalAreaCount(v)*multiplier;
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
    			fatArea = getPieceCount(v2, FAT_LOWER_BOUND, FAT_UPPER_BOUND)*multiplier;
    			partialArea = getPieceCount(v2, FAT_UPPER_BOUND, MUSCLE_LOWER_BOUND)*multiplier; 
    			leanArea = getPieceCount(v2, MUSCLE_LOWER_BOUND, MUSCLE_UPPER_BOUND)*multiplier; 
    			totalAreaCalc = getTotalAreaCalc(v2)*multiplier;
    			totalAreaCount = getTotalAreaCount(v2)*multiplier;
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
		
		public boolean isFinished() {
			return done;
		}
	
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
		
		private double getTotalAreaCalc(VOI v) {
			return v.area(); //returns volume in 3D
		}
	
		private double getTotalAreaCount(VOI v) {
			int totalArea = 0;
			BitSet fullMask = new BitSet();
			v.createBinaryMask(fullMask, getActiveImage().getExtents()[0], getActiveImage().getExtents()[1]);
			for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) 
		        totalArea++;
			return totalArea;
		}
	
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
			ArrayList<PlugInSelectableVOI> arr = new ArrayList();
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
	
		public VOI getCurrentVOI() {
			return currentVOI;
		}
	
		public void setCurrentVOI(VOI currentVOI) {
			this.currentVOI = currentVOI;
		}
	}
	/**
     * Gets all VOIs of given names from the buffer that have been created
     */
    
    public void getVOIs(String[] voiName, double fillVOIs) {
    	getActiveImage().unregisterAllVOIs();
    	ArrayList newName = new ArrayList();
    	String v;
    	for(int i=0; i<voiName.length; i++) {
    		if(voiBuffer.get(v = voiName[i].substring(0, voiName[i].lastIndexOf('.'))).isCreated())
    			newName.add(v);
    	}
    	
    	VOI tempVOI;
    	for(int i=0; i<newName.size(); i++) {
    		getActiveImage().registerVOI(tempVOI = voiBuffer.get(newName.get(i)));
    		if(fillVOIs != 0 && getZeroStatus((String)newName.get(i))) {
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
    
    private Color hasColor(VOI voi) {
        Color c = PlugInSelectableVOI.INVALID_COLOR;
        Iterator voiListItr = voiBuffer.keySet().iterator();
        boolean colorFound = false;
        String side1 = "", side2 = ""; 
        if(Symmetry.LEFT_RIGHT == Symmetry.LEFT_RIGHT) {
        	side1 = "Left";
        	side2 = "Right";
        }
        String testName = new String();
        while(voiListItr.hasNext() && !colorFound) {
        	testName = (String)voiListItr.next();
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
     * Loads VOI and automatically stores it into the voiBuffer.
     * @param name
     * @return
     */
    public PlugInSelectableVOI loadVOI(String name) {
    	String fileDir;
    	fileDir = getActiveImage().getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay.VOI_DIR+"\\";
    	String ext = name.contains(".xml") ? "" : ".xml";
    	PlugInSelectableVOI temp = voiBuffer.get(name);
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
    
    public PlugInSelectableVOI getSingleVOI(String name) {
    	return voiBuffer.get(name);
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
		fileDir = getActiveImage().getFileInfo(getViewableSlice()).getFileDirectory()+VOI_DIR+"\\";
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
			String center = ((String)fileInfo.getTagTable().getValue("0008,0080")).trim();
			mct2.addElement(new Paragraph((center.length() > 0 ? center : "      "), fontNormal));
			pdfDocument.add(mct2);
			
			MultiColumnText mct3 = new MultiColumnText(20);
			mct3.setAlignment(Element.ALIGN_LEFT);
			mct3.addRegularColumns(pdfDocument.left(), pdfDocument.right(), 10f, 4);
			mct3.addElement(new Paragraph("Patient ID:", fontBold));
			String id = ((String)fileInfo.getTagTable().getValue("0010,0020")).trim();
			mct3.addElement(new Paragraph((id.length() > 0 ? id : "      "), fontNormal));
			mct3.addElement(new Paragraph("Scan Date:", fontBold));
			String scanDate = ((String)fileInfo.getTagTable().getValue("0008,0020")).trim();
			mct3.addElement(new Paragraph((scanDate.length() > 0 ? scanDate : "      "), fontNormal));
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
			String height = ((String)fileInfo.getTagTable().getValue("0018,1130")).trim();
			spTable.addCell((height.length() > 0 ? height : "      "));
			
			
			
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
			String type = new String();
			if(multipleSlices) {
				aTable.addCell(new PdfPCell(new Paragraph("Volume (cm^3)", fontBold)));
				type = "Vol";	
			} else {
				aTable.addCell(new PdfPCell(new Paragraph("Area (cm^2)", fontBold)));
				type = "Area";
			}
			aTable.addCell(new PdfPCell(new Paragraph("Total "+type, fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Fat "+type, fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Lean "+type, fontBold)));
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
         *tp
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
        	//voiBuffer.get(voiName).setColor(c);
        	this.repaint();
        }
        
        public void selectedVOI(VOIEvent selection) {
        	/* not interested in having the ColorButon select VOIs */
        }
	}
	
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
	    	if(voiSearch(p)) {
	    		lastPt.x = stretchedPt.x;
	            lastPt.y = stretchedPt.y;
	        }
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

	public static final String VOI_DIR = "NIA_Seg";
}

//Old loadVOI
/*private void loadVOI(int pane) {
System.err.println("calling loadVOI");

//getImageA().unregisterAllVOIs();
String fileDir;


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
        	if(voiBuffer.get(name).getLocation() == pane) {
        		v = getSingleVOI(name+ext);
        		if(v != null) {
        			v.setThickness(2);
        			Color c = PlugInSelectableVOI.INVALID_COLOR;
        			
        			if((c = voiBuffer.get(v.getName()).getColor()).equals(PlugInSelectableVOI.INVALID_COLOR)) {
            			//System.out.println("A new one: "+v.getColor());
                    	if((c = hasColor(v)).equals(PlugInSelectableVOI.INVALID_COLOR))
                            v.setColor(c = colorPick[colorChoice++ % colorPick.length]);
                    	else
                    		v.setColor(c);
                    	voiBuffer.get(v.getName()).setColor(c);
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
}*/

/*Removed custom savesVOIs to
 * 
     * This method saves all VOIs for the active image to a given directory.  Note:  This method differs quite a bit in execution 
     * compared with saveALLVOIsTo(voiDir) in ViewJFrameBase.
     *
     * @param  voiDir  directory that contains VOIs for this image.
     
    public void saveAllVOIsTo(String voiDir, String currentVoi) {

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

    } // end saveAllVOIsTo()*/
