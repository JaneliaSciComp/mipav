import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogCaptureScreen;

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

/**
 * Creates an interface for working with Iceland CT images.
 * 
 * @author senseneyj
 *
 */


public class PlugInAlgorithmMuscleSegmentation extends AlgorithmBase {
    
    //~ Static fields --------------------------------------------------------------------------------------------------
    
    public static final Color[] colorPick = {Color.GREEN, Color.ORANGE, Color.CYAN, 
                                                Color.YELLOW, Color.MAGENTA};
    
    public static final String LUT_IMAGE = "lutImage.tif";
    public static final String VOI_IMAGE = "voiImage.tif";
    
    //~ Instance fields ------------------------------------------------------------------------------------------------    
    
    /** denotes the type of srcImg (see enum ImageType) */
    private ImageType imageType; 
    
    /** the parent frame. */
    private Frame parentFrame;
    
    /**The display*/
    private MuscleImageDisplay display;
    
    /**For writing PDF docs below. */
    private Document pdfDocument = null;
	private PdfWriter writer = null;
	private PdfPTable aTable = null;
	private PdfPTable imageTable = null;
	
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmMuscleSegmentation(ModelImage resultImage, ModelImage srcImg, ImageType imageType, Frame parentFrame) {
        super(resultImage, srcImg);
        this.imageType = imageType;
        this.parentFrame = parentFrame;
        //writeToPDF();
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        switch (imageType) {
            
            case ABDOMEN:
                performAbdomenDialog();
                break;
                
            case TWO_THIGHS:
                performThighDialog();
                break;
                
            default:
                displayError("Image type not supported");
                break;
               
        }
    } // end runAlgorithm()
    
    
    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }
    
    
    
    private void performAbdomenDialog() {
        
    	String[][] mirrorArr = new String[3][];
        mirrorArr[0] = new String[1];
        mirrorArr[0][0] = "Abdomen";
        
        mirrorArr[1] = new String[3];
        mirrorArr[1][0] = "Psoas";
        mirrorArr[1][1] = "Lateral Abdominal";
        mirrorArr[1][2] = "Paraspinous";
        
        mirrorArr[2] = new String[2];
        mirrorArr[2][0] = "Aortic calcium";
        mirrorArr[2][1] = "Rectus abdominus";
        
        boolean[][] mirrorZ = new boolean[3][];
        mirrorZ[0] = new boolean[1];
        mirrorZ[0][0] = false;
        
        mirrorZ[1] = new boolean[3];
        mirrorZ[1][0] = false;
        mirrorZ[1][1] = false;
        mirrorZ[1][2] = false;
        
        mirrorZ[2] = new boolean[2];
        mirrorZ[2][0] = false;
        mirrorZ[2][1] = true;
        
        String[][] noMirrorArr = new String[3][];
        noMirrorArr[0] = new String[1];
        noMirrorArr[0][0] = "Phantom";
        
        noMirrorArr[1] = new String[1];
        noMirrorArr[1][0] = "Bone sample";
        
        noMirrorArr[2] = new String[1];
        noMirrorArr[2][0] = "Water sample";
        
        boolean[][] noMirrorZ = new boolean[3][];
        noMirrorZ[0] = new boolean[1];
        noMirrorZ[0][0] = false;
        
        noMirrorZ[1] = new boolean[1];
        noMirrorZ[1][0] = false;
        
        noMirrorZ[2] = new boolean[1];
        noMirrorZ[2][0] = false;
        
        String[] titles = new String[3];
        titles[0] = "Thigh";
        titles[1] = "Bone";
        titles[2] = "Muscles";
        
        //needs better expl.
        
        boolean[] fillIn = new boolean[3]; //what is its effect on other images
        fillIn[0] = false; //everything outside should be filled
        fillIn[1] = true;  //everything inside should be filled
        fillIn[2] = false;  
        
        display = new MuscleImageDisplay(((ViewJFrameImage)parentFrame).getActiveImage(), titles, fillIn, mirrorArr, mirrorZ, 
                                                            noMirrorArr, noMirrorZ, ImageType.ABDOMEN, Symmetry.LEFT_RIGHT);
        
    }
    
    /**
     *   Builds thigh dialogue.
     */
    private void performThighDialog() {
        
        String[][] mirrorArr = new String[3][];
        mirrorArr[0] = new String[1];
        mirrorArr[0][0] = "Thigh";
        
        mirrorArr[1] = new String[2];
        mirrorArr[1][0] = "Bone";
        mirrorArr[1][1] = "Marrow";
        
        mirrorArr[2] = new String[5];
        mirrorArr[2][0] = "Fascia";
        mirrorArr[2][1] = "Quads";
        mirrorArr[2][2] = "Hamstrings";
        mirrorArr[2][3] = "Sartorius";
        mirrorArr[2][4] = "Adductors";
        
        boolean[][] mirrorZ = new boolean[3][];
        mirrorZ[0] = new boolean[1];
        mirrorZ[0][0] = false;
        
        mirrorZ[1] = new boolean[2];
        mirrorZ[1][0] = false;
        mirrorZ[1][1] = false;
        
        mirrorZ[2] = new boolean[5];
        mirrorZ[2][0] = false;
        mirrorZ[2][1] = true;
        mirrorZ[2][2] = true;
        mirrorZ[2][3] = true;
        mirrorZ[2][4] = true;
        
        String[][] noMirrorArr = new String[3][];
        noMirrorArr[0] = new String[1];
        noMirrorArr[0][0] = "Phantom";
        
        noMirrorArr[1] = new String[1];
        noMirrorArr[1][0] = "Bone sample";
        
        noMirrorArr[2] = new String[1];
        noMirrorArr[2][0] = "Water sample";
        
        boolean[][] noMirrorZ = new boolean[3][];
        noMirrorZ[0] = new boolean[1];
        noMirrorZ[0][0] = false;
        
        noMirrorZ[1] = new boolean[1];
        noMirrorZ[1][0] = false;
        
        noMirrorZ[2] = new boolean[1];
        noMirrorZ[2][0] = false;
        
        String[] titles = new String[3];
        titles[0] = "Thigh";
        titles[1] = "Bone";
        titles[2] = "Muscles";
        
        //needs better expl.
        
        boolean[] fillIn = new boolean[3]; //what is its effect on other images
        fillIn[0] = false; //everything outside should be filled
        fillIn[1] = true;  //everything inside should be filled
        fillIn[2] = false;  
        
        display = new MuscleImageDisplay(((ViewJFrameImage)parentFrame).getActiveImage(), titles, fillIn, mirrorArr, mirrorZ, 
                                                            noMirrorArr, noMirrorZ, ImageType.TWO_THIGHS, Symmetry.LEFT_RIGHT);
        
        
    }
    
    public enum ImageType{
        
        /** denotes that the srcImg is an abdomen */
        ABDOMEN,
        
        /** denotes that the srcImg is two thighs */
        TWO_THIGHS,
    }
    
    public enum Symmetry{
        
        /** Indicates the image has no symmetry. */
        NO_SYMMETRY, 
        
        /** Indicates that image has left-right symmetry. */
        LEFT_RIGHT, 
        
        /** Indicates the image has top-bottom symmetry. */
        TOP_BOTTOM
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
    	
    	private String buttonStringGroup[] = {OK, CLEAR, HELP};
    	
    	protected JButton buttonGroup[];
    	
    	protected MuscleImageDisplay parentFrame;
    	
    	private Vector objectList = new Vector();
    	
    	public DialogPrompt(MuscleImageDisplay theParentFrame) {
    		this.parentFrame = theParentFrame;
    	}
    	
    	public DialogPrompt(MuscleImageDisplay theParentFrame, String[] buttonString) {
    		this.parentFrame = theParentFrame;
    		this.buttonStringGroup = buttonString;
    	}
    	
    	protected void setButtons(String[] buttonString) {
    		this.buttonStringGroup = buttonString;
    	}
    	
    	protected abstract void initDialog();
    	
    	/**
         * Builds button panel consisting of OK, Cancel and Help buttons.
         *
         * @return  JPanel that has ok, cancel, and help buttons
         */
        protected JPanel buildButtons() {
            JPanel buttonPanel = new JPanel();
            buttonGroup = new JButton[buttonStringGroup.length];
            
            if (buttonGroup.length > 3) {
            	JPanel topPanel = new JPanel();
            	JPanel bottomPanel = new JPanel();
            	for(int i=0; i<buttonStringGroup.length; i++) {
            		buttonGroup[i] = new JButton(buttonStringGroup[i]);
            		buttonGroup[i].addActionListener(parentFrame);
            		buttonGroup[i].addActionListener(this);
            		buttonGroup[i].setActionCommand(buttonStringGroup[i]);
            		if (buttonStringGroup[i].length() < 10) { 
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
            
            	for(int i=0; i<buttonStringGroup.length; i++) {
            		buttonGroup[i] = new JButton(buttonStringGroup[i]);
            		buttonGroup[i].addActionListener(parentFrame);
            		buttonGroup[i].addActionListener(this);
            		buttonGroup[i].setActionCommand(buttonStringGroup[i]);
            		if (buttonStringGroup[i].length() < 10) { 
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
        
        /**Whether this dialog completed. */
        private boolean completed;
        
        /**Whether this dialog produced a novel Voi. */
        private boolean novelVoiProduced;

        public VoiDialogPrompt(MuscleImageDisplay theParentFrame) {
            super(theParentFrame);
           
            setButtons(buttonStringList);
        }
        
        private boolean voiExists(String objectName) {
        	if(display == null) {
        		int i=0;
        	} else if(display.getActiveImage() == null) {
        		int j=0; //makes more sense
        	}
        	
        	String fileDir = display.getActiveImage().getFileInfo(0).getFileDirectory()+MuscleImageDisplay.VOI_DIR;
            
            if(new File(fileDir+objectName+".xml").exists()) {
                return true;
            } 
            return false;
        }
        
        public void actionPerformed(ActionEvent e) {

            String command = e.getActionCommand();

            if(command.equals(CLEAR)) {
                //clear all VOIs drawn
                display.getActiveImage().unregisterAllVOIs();
                display.updateImages(true);
            } else if (command.equals(OK)) {
                    VOI goodVoi = checkVoi();
                    //check that VOI conforms to requirements, returns the VOI being modified/created
                    if ( goodVoi != null ) { 
                        
                        //save modified/created VOI to file
                        display.getActiveImage().unregisterAllVOIs();
                        display.getActiveImage().registerVOI(goodVoi);
                        String dir = display.getActiveImage().getImageDirectory()+MuscleImageDisplay.VOI_DIR;
                        display.saveAllVOIsTo(dir);
                        
                        String fileDir = display.getActiveImage().getFileInfo(0).getFileDirectory();

                        MipavUtil.displayInfo(objectName+" VOI saved in folder\n " + fileDir + MuscleImageDisplay.VOI_DIR);
                        
                        display.getActiveImage().unregisterAllVOIs();
                        display.updateImages(true);
                        
                        notifyListeners(OK);
                    } else {
                        MipavUtil.displayError("Note that no VOI has been saved due to previous error.");
                    }
                } else if (command.equals(CANCEL)) {
                    setCompleted(savedVoiExists());
                    notifyListeners(CANCEL);
                    //dispose();
                } else if (command.equals(HELP)) {
                    MipavUtil.showHelp("19014");
                }
            }
            
        
        
        public void takeDownDialog() {
        	removeAll();
        	
        	completed = true;
        }
        
        public void setUpDialog(String name, boolean closedVoi, int numVoi) {	
        	this.objectName = name;
        	this.closedVoi = closedVoi;
        	this.numVoi = numVoi;
        	
        	voiExists = voiExists(objectName);
            
            novelVoiProduced = false;
            completed = false;
            
            initDialog();
        }
        

        public String getObjectName() {
            return objectName;
        }
        
        public boolean isCompleted() {
            return completed;
        }
        
        public boolean isNovel() {
            return novelVoiProduced;
        }
        
        private boolean savedVoiExists() {
            String fileName = new String(parentFrame.getImageA().getFileInfo(0).getFileDirectory()+MuscleImageDisplay.VOI_DIR+objectName+".xml");
            return new File(fileName).exists();
        }
        
        
        
        /**
         * Initializes the dialog box.
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
                
            
            
            String closedStr = closedVoi ? "closed " : "";
            String pluralVOI = numVoi > 1 ? "s" : "";
            String existStr = voiExists ? "Modify the" : "Create";
            
            String voiStr = new String(existStr+" "+numVoi+" "+closedStr+"VOI curve"+pluralVOI+" around the "+
                                        objectName.toLowerCase()+".");
            JLabel label = new JLabel(voiStr);
            
            label.setFont(MipavUtil.font12);
            mainPanel.add(label, BorderLayout.NORTH);
            
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
            VOIVector srcVOI = parentFrame.getActiveImage().getVOIs();
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
	            	if(display.getLocationStatus(srcVOI.get(i).getName()) == -1) {
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
    
    /**
     * @author senseneyj
     * 
     * Test class for image to confirm correct behavior.
     * 
     * TODO: Incorporate into MuscleIMageDisplay
     */
    
    public class MuscleImageDisplay extends ViewJFrameImage {
        
    	//private AnalysisPrompt analysisPrompt;

        //public static final int REMOVED_INTENSITY = -2048;
        
        public static final String CHECK_VOI = "CHECK_VOI";
        
        public static final String VOI_DIR = "NIA_Seg\\";
        
        private int voiTabLoc;
        
        private int resultTabLoc;
        
        //private int voiIndex; not needed since seperate classes now
        
        //private int componentShown = 0;

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
        
        private boolean[] fillIn;
        
        private TreeMap fillStatus;
        
        private TreeMap locationStatus;
        
        private BuildThighAxes thighAxes;
        
        public MuscleImageDisplay(ModelImage image, String[] titles, boolean[] fillIn,
                String[][] mirrorArr, boolean[][] mirrorZ, 
                String[][] noMirrorArr, boolean[][] noMirrorZ,  
                ImageType imageType, Symmetry symmetry) {

            super(image);
            this.setImageA(image);
            this.setActiveImage(IMAGE_A);
            this.fillIn = fillIn;
            this.titles = titles;
            this.mirrorArr = mirrorArr;
            this.mirrorZ = mirrorZ;
            this.noMirrorArr = noMirrorArr;
            this.noMirrorZ = noMirrorZ;
            this.imageType = imageType;
            this.symmetry = symmetry;
            
            fillStatus = new TreeMap();
            locationStatus = new TreeMap();
            
            if (imageA == null) {
                return;
            }
            
            initNext();

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
            super.actionPerformed(e);
            String command = e.getActionCommand();
            System.out.println("Caught2: "+command);
            if(command.equals(MuscleImageDisplay.CHECK_VOI)) {
                //initVoiImage();
                String voiString = ((JButton)(e.getSource())).getText();
                ((VoiDialogPrompt)tabs[voiTabLoc]).setUpDialog(voiString, true, 1);
                // This is very important. Adding this object as a listener allows the subdialog to
                // notify this object when it has completed or failed. 
                // This is could be generalized by making a subDialog interface.
                lockToPanel(voiTabLoc, "VOI"); //includes making visible
                //setVisible(false); // Hide dialog
            } else if(command.equals(DialogPrompt.CALCULATE)) {
            	//display the result display prompt in same way
            	//System.out.println("OK Command, proceed with analysis.");
            	//analysisPrompt = new AnalysisPrompt(this, mirrorArr, noMirrorArr, symmetry);
            	// This is could be generalized by making a subDialog interface.
            	//analysisPrompt.addListener(this);
            	//analysisPrompt.addComponentListener(this);
            	
            	
            	lockToPanel(resultTabLoc, "Analysis"); //includes making visible
            	
            } else if (!(command.equals(DialogPrompt.OUTPUT) ||
            		command.equals(DialogPrompt.SAVE) ||
            		command.equals(DialogPrompt.OUTPUT_ALL))) {
            	if(command.equals(DialogPrompt.CANCEL)) {
            		unlockToPanel(voiTabLoc);
            		initMuscleImage(activeTab);
            	} else if(command.equals(DialogPrompt.CLEAR)) {
            		getActiveImage().unregisterAllVOIs();
            		updateImages(true);
            	} else if(command.equals(DialogPrompt.OK)) {
            		unlockToPanel(voiTabLoc);
            		initMuscleImage(activeTab);
            	} else if(command.equals(DialogPrompt.BACK)) {
            		unlockToPanel(resultTabLoc);
            		initMuscleImage(activeTab);
            	}
            } 
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
            	tabs[i] = new MuscleDialogPrompt(this, mirrorArr[i], mirrorZ[i],
                        noMirrorArr[i], noMirrorZ[i],
                        imageType, symmetry);

            	tabs[i].addComponentListener(this);
            	tabs[i].addListener(this);
            	tabs[i].setName(titles[i]);
            	imagePane.addTab((i+1)+": "+titles[i], tabs[i]);

            	zeroStatus.putAll(((MuscleDialogPrompt)tabs[i]).getZeroStatus());
            	fillStatus.put(titles[i], fillIn[i]);
            	
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
            tabs[resultTabLoc] = new AnalysisPrompt(this, mirrorArr, noMirrorArr, symmetry);
            tabs[resultTabLoc].addListener(this);
            tabs[resultTabLoc].addComponentListener(this);
            tabs[resultTabLoc].setVisible(false);
            
            return panelA;
            
        }
        
        private void initNext() {
            JPanel panelA = initDialog();
            
            getContentPane().add(panelA);
            getContentPane().remove(0);
            //Container c = getContentPane();
            
            pack();
            initMuscleImage(0);
            this.setResizable(true);
        }
        
        @Override
		public void componentHidden(ComponentEvent event) {
		    //System.out.println("Selected pane: "+imagePane.getSelectedIndex()+"\tVOI pane: "+voiIndex);
		    //System.out.println("Active pane: "+activeTab);
		    //Component c = event.getComponent();
		    //Object obj = event.getSource();
		    //if(imagePane != null) {
		    //    if(event.getComponent().equals(tabs[activeTab])) {
		            //getActiveImage().unregisterAllVOIs();
		            //updateImages(true);
		    //    }
		    //}
		    super.componentHidden(event);
		}

		@Override
		public void componentShown(ComponentEvent event) {
		    Component c = event.getComponent();
		    if(c instanceof MuscleDialogPrompt) {
		    	for(int i=0; i<voiTabLoc; i++) {
		    		if(tabs[i].equals(c)) {
		    			initMuscleImage(i);
		    			activeTab = i;
		    		}
		    	}
		    } else if(c instanceof VoiDialogPrompt && activeTab != voiTabLoc) {
		    	initVoiImage(activeTab); //replacing current image and updating
		    	//activeTab = voiTabLoc;
		    } else if(c instanceof AnalysisPrompt && activeTab != resultTabLoc) {
		    	getActiveImage().unregisterAllVOIs();
		    	updateImages(true);
		    	//activeTab = resultTabLoc;
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
        
        public void lockToPanel(int tabLoc, String title) {
            //saving selected tab to get back to
        	activeTab = imagePane.getSelectedIndex();
        	if(tabLoc >= voiTabLoc) {
            	tabs[tabLoc].setVisible(true);
            	for(int i=0; i<voiTabLoc; i++)
            		imagePane.setEnabledAt(i, false);
            	imagePane.addTab(title, tabs[tabLoc]);
            	imagePane.setSelectedIndex(voiTabLoc);
            } 
        }
        
        public void unlockToPanel(int closingTabLoc) {
        	tabs[closingTabLoc].setVisible(false);
        	imagePane.remove(tabs[closingTabLoc]);
            for(int i=0; i<voiTabLoc; i++) 
                imagePane.setEnabledAt(i, true);
            imagePane.setSelectedIndex(activeTab);
            
        }
        
        public int getLocationStatus(String name) {
        	int loc = -1;
        	if(locationStatus.get(name) != null)
        		loc = (Integer)locationStatus.get(name);
        	return loc;
        }
        
        public boolean getZeroStatus(String name) {
        	boolean fill = false;
        	if(zeroStatus.get(name) != null)
        		fill = (Boolean)zeroStatus.get(name);
        	return fill;
        }
        
        
        //TODO: Local copies of VOIs
        
        private void loadVOI(int pane) {
            int colorChoice = 0;
        	getImageA().unregisterAllVOIs();
            String fileDir = getImageA().getFileInfo(0).getFileDirectory()+VOI_DIR;
            File allVOIs = new File(fileDir);
            if(allVOIs.isDirectory()) {
                String[] voiName = allVOIs.list();
                for(int i=0; i<voiName.length; i++) {
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
                			getActiveImage().registerVOI(v);
                		}
                	}
                }
            }
        }
        
        /*private void initVOIColor() {
            int colorChoice = 0;
            VOIVector voiFullVec = getActiveImage().getVOIs();
            for(int i=0; i<voiFullVec.size(); i++) {
            	Color c = null;
            	if((c = hasColor(voiFullVec.get(i))) == null)
                    voiFullVec.get(i).setColor(colorPick[colorChoice++ % colorPick.length]);
            	else
            		voiFullVec.get(i).setColor(c);
            }
        }*/
        
        private Color hasColor(VOI voiVec) {
            Color c = null;
            VOIVector tempVec = display.getActiveImage().getVOIs();
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
        
        private void initMuscleImage(int pane) {        
            ///if pane == -1, will load all VOIs
            loadVOI(pane);
            //initVOIColor();
            
            ctMode(getImageA(), -175, 275);
            
            //if(pane == 0){
            //    thighAxes = new BuildThighAxes(getImageA(), 0);
            //    thighAxes.createAxes();
            //} //else they're loaded in loadVOI
            //added before button check so that they can be accessed in this way, optional to change.
            
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
        	//set VOIs to correct colors
        	//initVOIColor();
        	//make all other VOIs solid
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
                        updateImages(true);
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
                    
                
                    //Color voiColor = thighVOIs[i].getColor();
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

        
        
        /** Three arrays to save the coordinates of the LUT's transfer fucntion. z[] not used. */
        private float[] x = new float[4];

        /** DOCUMENT ME! */
        private float[] y = new float[4];

        /** DOCUMENT ME! */
        private float[] z = new float[4];
        
        /**
         * Reference to the image data of the slice presently displayed. Needed to calculate the max/min of the slice used
         * to adjust the transfer function.
         */
        private float[] dataSlice;
        
        /** Image's minimum intensity. */
        private float minImage;
        
        /** Image's maximum intensity. */
        private float maxImage;
        
        /** DOCUMENT ME! */
        protected Dimension dim = new Dimension(256, 256);
        
        /**
         * Sets mode to CT and sets range to CT presets.
         *
         * @param  preset1  first CT preset
         * @param  preset2  second CT preset
         */
        public void ctMode(ModelImage image, int preset1, int preset2) {
            float min = Float.MAX_VALUE;
            float max = -Float.MIN_VALUE;
            int i;
            
            ModelLUT LUT = getComponentImage().getLUTa();
          
            calcMinMax(image);
            
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
            //((ViewJFrameImage) parentFrame).updateWinLevel((int) (x[1] + 0.5), (int) (x[2] + 0.5));
            LUT.getTransferFunction().importArrays(x, y, 4);
            image.notifyImageDisplayListeners(LUT, false);

            //if (image.getHistoLUTFrame() != null) {
            //    updateHistoLUTFrame();
            //}
        }
        
        /**
         * Calculate the maximum and minimum valuse to setup the window and level sliders.
         */
        private void calcMinMax(ModelImage image) {

            if (image.getType() == ModelStorageBase.UBYTE) {
                minImage = 0;
                maxImage = 255;
            } else if (image.getType() == ModelStorageBase.BYTE) {
                minImage = -128;
                maxImage = 127;
            } else {
                minImage = (float) image.getMin();
                maxImage = (float) image.getMax();
            }
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
        
        //private ModelImage srcImg;
        
        /**
         * Creates new set of prompts for particular muscle.
         *
         * @param  theParentFrame  Parent frame.
         */
        public MuscleDialogPrompt(MuscleImageDisplay theParentFrame, String[] mirrorArr, boolean[] mirrorZ, 
                String[] noMirrorArr, boolean[] noMirrorZ,  
                ImageType imageType, Symmetry symmetry) {
            //super(theParentFrame, false);
            super(theParentFrame);
            
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
            
            String command = e.getActionCommand();
            System.out.println("Caught1: "+command);
            
            
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
            //resizing bug fixed, extra not necessary
            
            for(int i=0; i<instructionLabel.length; i++) {
                instructionLabel[i].setFont(MipavUtil.font12);
                instructionPanel.add(instructionLabel[i], gbc);
                gbc.gridy++;
            }
            
            return instructionPanel;
        }
        
        private JPanel initSymmetricalObjects() {
            
            VOIVector existingVois = parentFrame.getImageA().getVOIs();
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
                mirrorButtonArr[i].setActionCommand(MuscleImageDisplay.CHECK_VOI);
                mirrorButtonArr[i].addActionListener(parentFrame);
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
                
                //System.out.println(mirrorButtonArr[i].getText()+" is "+mirrorZ[i/2]);
                zeroStatus.put(mirrorButtonArr[i].getText(), mirrorZ[i/2]);
            
            }          
            return mirrorPanel;
                    
        }
        
        private JPanel initNonSymmetricalObjects() {
            VOIVector existingVois = parentFrame.getImageA().getVOIs();
            
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
                noMirrorButtonArr[i].setActionCommand(MuscleImageDisplay.CHECK_VOI);
                noMirrorButtonArr[i].addActionListener(parentFrame);
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
        
        private boolean checkVariables() {
            boolean done = true;
            for(int i=0; i<mirrorCheckArr.length; i++) {
                if(!mirrorCheckArr[i].isSelected()) {
                    done = false;
                }
            }
            for(int i=0; i<mirrorCheckArr.length; i++) {
                if(!mirrorCheckArr[i].isSelected()) {
                    done = false;
                }
            }
            return done;
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
	
		private Symmetry symmetry;
	
		/**
		 * Labels for instructions. 
		 */
		private JLabel[] instructionLabel;

		/**
		 * Text for muscles where a mirror muscle may exist. 
		 */
		private String[][] mirrorArr;

		/**
		 * Text for muscles where mirror muscles are not considered. 
		 */
		private String[][] noMirrorArr;
		
		private JList[] list;

		/**
		 * Vector list of objects that listen to this dialog box. When the action for this pseudo-algorithm has
		 * completed, the program will notify all listeners.
		 */
		//private Vector objectList = new Vector();
		
		private String name[] = {"thigh", "bone component", "muscle"};
		
		private int time = 0;
		
		private boolean lutOn = false;
		
		public AnalysisPrompt(MuscleImageDisplay theParentFrame, String[][] mirrorArr, String[][] noMirrorArr, Symmetry symmetry) {
	        super(theParentFrame);
	        
	        setButtons(buttonStringList);
	        
	        this.noMirrorArr = noMirrorArr;
	        this.mirrorArr = mirrorArr;
	        
	        this.symmetry = symmetry;
	        
	        
	        
	        
	        initDialog();	        
	    }
		
		/**
		 * Loads the CT Thigh specific lut
		 *
		 */
		private void loadLUT() {
			float min = (float)display.getActiveImage().getMin();
			float max = (float)display.getActiveImage().getMax();
			
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
			
			display.getLUTa().makeCTThighTransferFunctions();
			display.getLUTa().setTransferFunction(transfer);
			display.getLUTa().makeLUT(256);
			
			VOIVector vec = display.getActiveImage().getVOIs();
			for(int i=0; i<vec.size(); i++) {
				if(display.getZeroStatus(vec.get(i).getName())) {
                	vec.get(i).setDisplayMode(VOI.SOLID);
                	vec.get(i).setOpacity((float)0.7);
                }
			}
			
			display.updateImages(true);
			display.getActiveImage().getParentFrame().updateImages(true);
			
			lutOn = true;
		}
		
		private void removeLUT() {
			
			display.ctMode(display.getActiveImage(), -175, 275);
			
			display.getLUTa().makeGrayTransferFunctions();
			display.getLUTa().makeLUT(256);
			
			VOIVector vec = display.getActiveImage().getVOIs();
			for(int i=0; i<vec.size(); i++) 
				vec.get(i).setDisplayMode(VOI.CONTOUR);
			
			display.updateImages(true);
			
			lutOn = false;
		}
		
		/**
		 * sets all the VOIs to solid-type
		 *
		 */
		private void solidifyVOIs() {
			VOIVector VOIs = display.getActiveImage().getVOIs();
			for (int i = 0; i < VOIs.size(); i++) {
				VOIs.elementAt(i).setDisplayMode(VOI.SOLID);
			}			
		}
		
		/**
	     * Initializes the dialog box.
	     *
	     */
	    
		protected void initDialog() {
	        setForeground(Color.black);
	        //zeroStatus = new TreeMap();
	        
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
	        instructionLabel[0] = new JLabel("1) Click on the muscle you would like to display.\n\r");
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
	        //VOIVector existingVois = ((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).getVOIs();
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
	            noMirrorButtonArr[i].setActionCommand(MuscleImageDisplay.CHECK_VOI);
	            noMirrorButtonArr[i].addActionListener(parentFrame);
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
	        String command = e.getActionCommand();
	        if(command.equals(CLEAR)) {
	            //clear all VOIs drawn
	            parentFrame.getImageA().unregisterAllVOIs();
	            parentFrame.updateImages();
	        } else {
	
	            if (command.equals(OK)) {
	                VOI goodVoi = null; //was checkVOI)_
	                //check that VOI conforms to requirements, returns the VOI being modified/created
	                
	                if ( goodVoi != null ) { 
	                    
	                    //save modified/created VOI to file
	                    parentFrame.getImageA().unregisterAllVOIs();
	                    parentFrame.getImageA().registerVOI(goodVoi);
	                    String dir = parentFrame.getImageA().getImageDirectory()+MuscleImageDisplay.VOI_DIR;
	                    parentFrame.saveAllVOIsTo(dir);
	                    
	                    String fileDir = parentFrame.getImageA().getFileInfo(0).getFileDirectory();
	
	                    MipavUtil.displayInfo(/*objectName*/"test"+" VOI saved in folder\n " + fileDir + MuscleImageDisplay.VOI_DIR);
	                    
	                    parentFrame.getImageA().unregisterAllVOIs();
	                    parentFrame.updateImages();
	                    
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
	            	processCalculations(true, true);
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
		    	parentFrame.updateImages(true);
		    	time = 0;
	    	} else 
	    		time++;
	    	
	    }
	    
	    	    
	    private void processCalculations(boolean all, boolean doSave) {
	    	boolean pdfCreated = false;
	    	for (int listNum = 0; listNum < list.length; listNum++) {
	    		ListModel model = list[listNum].getModel();		    	
	    		String [] listStrings = null;
	    		if (all) {
	    			listStrings = new String[model.getSize()];
	    			for (int i = 0; i < listStrings.length; i++) {
		    			listStrings[i] = (String)model.getElementAt(i) + ".xml";
		    		}
	    		} else {
	    			Object[] selected = list[listNum].getSelectedValues();
	    			listStrings = new String[selected.length];
	    			for(int i=0; i<selected.length; i++) {
	    				listStrings[i] = (String)selected[i]+".xml";
			    	}
	    		}
	    		
	    		if (listStrings.length > 0) {
	    			
	    			//if PDF hasnt been created and we're saving, create it now
	    			if (doSave && !pdfCreated) {
	    				if (!createPDF()) {
	    					return;
	    				}
	    				pdfCreated = true;
	    			}
	    			//ModelLUT lut;
	    			//Load VOIs and calculations
	    			loadVOIs(listStrings, lutOn);
	    			//parentFrame.updateImages(true);
	    			//Image now contains all valid VOIs, display calculations
	    			VOIVector voi = parentFrame.getActiveImage().getVOIs();
	    			for(int i=0; i<voi.size(); i++) {
	    				VOI v = voi.get(i);
	    				double totalAreaCalc = 0, totalAreaCount = 0, fatArea = 0, leanArea = 0;//, partialArea = 0;
	    				double meanFatH = 0, meanLeanH = 0, meanTotalH = 0;
	    				//pixels -> cm^2
	    				totalAreaCalc = getTotalAreaCalc(v)*(Math.pow(parentFrame.getActiveImage().getResolutions(0)[0]*.1, 2));
	    				totalAreaCount = getTotalAreaCount(v)*(Math.pow(parentFrame.getActiveImage().getResolutions(0)[0]*.1, 2));
	    				fatArea = getFatArea(v)*(Math.pow(parentFrame.getActiveImage().getResolutions(0)[0]*.1, 2));
	    				leanArea = getLeanArea(v)*(Math.pow(parentFrame.getActiveImage().getResolutions(0)[0]*.1, 2));
	    				//partialArea = getPartialArea(v)*(Math.pow(parentFrame.getActiveImage().getResolutions(0)[0]*.1, 2));
	    				meanFatH = getMeanFatH(v);
	    				meanLeanH = getMeanLeanH(v);
	    				meanTotalH = getMeanTotalH(v);
	    				
	    				System.out.println("Compare areas: "+totalAreaCalc+"\tcount: "+totalAreaCount);
	    				
	    				if (doSave) {
	    					addToPDF(listStrings[i], fatArea, leanArea, totalAreaCount, meanFatH, meanLeanH, meanTotalH);
	    				} else {
	    					DecimalFormat dec = new DecimalFormat("0.#");
		    				String appMessage = v.getName()+" calculations:\n"+"Fat Area: "+dec.format(fatArea)+
		    				"\t\t\t\tMean H: "+dec.format(meanFatH)+"\nLean Area: "+dec.format(leanArea)+
		    				"\t\t\tMean H: "+dec.format(meanLeanH)+"\nTotal Area: "+dec.format(totalAreaCount)+
		    				"\t\t\tMean H: "+dec.format(meanTotalH) + "\n\n";
		    			
		    				ViewUserInterface.getReference().getMessageFrame().append(appMessage, ViewJFrameMessage.DATA);
	    				}	
	    			}
	    		}
	    	}
	    	
	    	if (doSave) {
	    		
	    		display.getActiveImage().getParentFrame().requestFocus();
	    		
	    		
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
	    		loadVOIs(allStrings, false);
	    		//solidifyVOIs();
	    		display.getActiveImage().getParentFrame().updateImages(true);
	    		
	    		java.awt.Image qaImage = captureImage();
	    		
	    		loadVOIs(new String[] {}, false);
	    		loadLUT();
	    		
	    		java.awt.Image edgeImage = captureImage();
	    		
	    		closePDF(edgeImage, qaImage);
	    	}
	    	
	    }
	    	    	    
	    
	    
	    
	}
    
    private java.awt.Image captureImage() {
    	Rectangle currentRectangle;
    	Point p = new Point();
    	p.x = 0;
        p.y = 0;
        SwingUtilities.convertPointToScreen(p, display.getActiveImage().getParentFrame().getContentPane());
        p.x++; // must correct this slightly
        p.y++; // ""

        Dimension d = new Dimension();
        d.width = display.getActiveImage().getParentFrame().getContentPane().getWidth() - 3; // the -3 is a correction
        d.height = display.getActiveImage().getParentFrame().getContentPane().getHeight() - 3; // ""
        currentRectangle = new Rectangle(p, d);
        
        try {
            Robot robot = new Robot();

            return robot.createScreenCapture(currentRectangle);
        } catch (OutOfMemoryError error) {
        } catch (AWTException error) {
        }
        return null;
    }
 
    public void loadVOIs(String[] voiName, boolean fillVOIs) {
    	if (display == null) {
    		return;
    	}
        display.getActiveImage().unregisterAllVOIs();
        int colorChoice = new Random().nextInt(colorPick.length);
        String fileDir = display.getActiveImage().getFileInfo(0).getFileDirectory()+MuscleImageDisplay.VOI_DIR;
        File allVOIs = new File(fileDir);
        if(allVOIs.isDirectory()) {
            for(int i=0; i<voiName.length; i++) {
                //System.out.println(voiName[i]);

                if(new File(fileDir+voiName[i]).exists()) {
                    String fileName = voiName[i];
                    FileVOI v;
                    VOI[] voiVec = null;
                    try {
                        v = new FileVOI(fileName, fileDir, display.getActiveImage());
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
                        if(fillVOIs && display.getZeroStatus(voiVec[0].getName())) {
                        	voiVec[0].setDisplayMode(VOI.SOLID);
                        	voiVec[0].setOpacity((float)0.7);
                        }
                        display.getActiveImage().registerVOI(voiVec[0]);
                    }
                }
            }
        }  
    }
    
    private Color hasColor(VOI voiVec) {
        Color c = null;
        VOIVector tempVec = display.getActiveImage().getVOIs();
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
    	if(display == null) {
    		return null;
    	} else if(display.getActiveImage() == null) {
    		int j = 1;
    	}
        String fileDir = display.getActiveImage().getFileInfo(0).getFileDirectory()+MuscleImageDisplay.VOI_DIR;
        String ext = name.contains(".xml") ? "" : ".xml";
        
        if(new File(fileDir+name+ext).exists()) {
            FileVOI v;
            VOI[] voiVec = null;
            try {
                v = new FileVOI(name+ext, fileDir, display.getActiveImage());
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
        
    public int getFatArea(VOI v) {
    	int fatArea = 0;
    	BitSet fullMask = new BitSet();
    	v.createBinaryMask(fullMask, display.getActiveImage().getExtents()[0], display.getActiveImage().getExtents()[1]);
    	double mark = 0;
    	for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
            mark = display.getImageA().getDouble(i);
    		if(mark  >= -190 && mark <= -30) 
    			fatArea++;
    	}
    	return fatArea;
    }
    
    public int getPartialArea(VOI v) {
    	int partialArea = 0;
    	BitSet fullMask = new BitSet();
    	v.createBinaryMask(fullMask, display.getActiveImage().getExtents()[0], display.getActiveImage().getExtents()[1]);
    	double mark = 0;
    	for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
            mark = display.getImageA().getDouble(i);
    		if(mark  >= -30 && mark <= 0) 
    			partialArea++;
    	}
    	return partialArea;
    }
    
    public int getLeanArea(VOI v) {
    	int leanArea = 0;
    	BitSet fullMask = new BitSet();
    	v.createBinaryMask(fullMask, display.getActiveImage().getExtents()[0], display.getActiveImage().getExtents()[1]);
    	double mark = 0;
    	for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
            mark = display.getImageA().getDouble(i);
    		if(mark  >= 0 && mark <= 100) 
    			leanArea++;
    	}
    	return leanArea;
    }
    
    //TODO: Partial voluming difference between VOI calculations and counting
    
    public int getTotalAreaCalc(VOI v) {
    	return v.area();
    }
    
    public int getTotalAreaCount(VOI v) {
    	int totalArea = 0;
    	BitSet fullMask = new BitSet();
    	v.createBinaryMask(fullMask, display.getActiveImage().getExtents()[0], display.getActiveImage().getExtents()[1]);
    	for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) 
            totalArea++;
    	return totalArea;
    }
    
    public double getMeanFatH(VOI v) {
    	int fatArea = 0;
    	double meanFatH = 0;
    	BitSet fullMask = new BitSet();
    	v.createBinaryMask(fullMask, display.getActiveImage().getExtents()[0], display.getActiveImage().getExtents()[1]);
    	double mark = 0;
    	for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
            mark = display.getImageA().getDouble(i);
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
    	v.createBinaryMask(fullMask, display.getActiveImage().getExtents()[0], display.getActiveImage().getExtents()[1]);
    	double mark = 0;
    	for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
            mark = display.getImageA().getDouble(i);
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
    	v.createBinaryMask(fullMask, display.getActiveImage().getExtents()[0], display.getActiveImage().getExtents()[1]);
    	double mark = 0;
    	for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
            mark = display.getImageA().getDouble(i);
            totalArea++;
			meanTotalH += mark;
    	}
    	meanTotalH /= totalArea;
    	return meanTotalH;
    }
    
    
    private boolean createPDF() {
    	JFileChooser chooser = new JFileChooser();
		chooser.setDialogTitle("Save to PDF");
		chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { "pdf" }));
		
		File file = null;
		int returnVal = chooser.showSaveDialog(null);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
           file = chooser.getSelectedFile();
        } else {
        	return false;
        }
		
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
          
			
			Font fontNormal = FontFactory.getFont("Helvetica", 10, Font.NORMAL, Color.DARK_GRAY);
			Font fontBold = FontFactory.getFont("Helvetica", 10, Font.BOLD, Color.BLACK);

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
			mct2.addElement(new Paragraph(display.getActiveImage().getFileInfo()[0].getFileName(), fontNormal));
			mct2.addElement(new Paragraph("Center:", fontBold));
			mct2.addElement(new Paragraph("Hjartavernd", fontNormal));
			FileInfoBase[] f = display.getActiveImage().getFileInfo();
			pdfDocument.add(mct2);
			
			MultiColumnText mct3 = new MultiColumnText(20);
			mct3.setAlignment(Element.ALIGN_LEFT);
			mct3.addRegularColumns(pdfDocument.left(), pdfDocument.right(), 10f, 4);
			mct3.addElement(new Paragraph("ID:", fontBold));
			mct3.addElement(new Paragraph(display.getActiveImage().getFileInfo()[0].getFileName(), fontNormal));
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
			spTable.addCell(Double.toString(display.getActiveImage().getResolutions(0)[0]*.1));
			spTable.addCell("Slice Thickness: (mm)");
			spTable.addCell(Float.toString(display.getActiveImage().getFileInfo()[0].getSliceThickness()));
			spTable.addCell("Table Height: (cm)");
			spTable.addCell("143.00");
			
			Paragraph pTable = new Paragraph();
			pTable.add(new Paragraph());
			pTable.setAlignment(Element.ALIGN_CENTER);
			pTable.add(spTable);
			pdfDocument.add(new Paragraph(new Chunk("")));
			pdfDocument.add(pTable);
			
			//create the Table where we will insert the data:
			aTable = new PdfPTable(7);
			
			// add Column Titles (in bold)
			aTable.addCell(new PdfPCell(new Paragraph("Area (cm^2)", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Total Area", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Fat Area", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Lean Area", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Fat HU", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Lean HU", fontBold)));
			aTable.addCell(new PdfPCell(new Paragraph("Total HU", fontBold)));
			
			
			return true;
		} catch (Exception e) {
			return false;
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
	private void addToPDF(String name, double fatArea, double leanArea, double totalAreaCount, 
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
			
			String fileDir = display.getActiveImage().getFileInfo(0).getFileDirectory()+MuscleImageDisplay.VOI_DIR;
			
			//loadLut
			
			//imageTable.addCell(Image.getInstance(chooser.getSelectedFile().getPath()));
			
			//load all VOIs
			
	        //imageTable.addCell(Image.getInstance(chooser.getSelectedFile().getPath()));
					
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private void closePDF(java.awt.Image edgeImage, java.awt.Image qaImage) {
		try {
		Paragraph aPar = new Paragraph();
		aPar.setAlignment(Element.ALIGN_CENTER);
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
    
}

/*************************************************************************
 * Removed code from initVoiImage, functionality includes zeroing out
 * areas not needed, mask coverage, and creating seperate images for different tasks.
 * @param voiVec
 * @return
 */
//VOIVector vector = srcImage.getVOIs();
//VOI removedVoi = null;

//for(int i=0; i < vector.size() ; i++) {
	//TODO: debug
	//Find same voi, and remove it from original image
    //if(((VOI)vector.get(i)).getName().equals(voiDialog.getObjectName())) {
    //    removedVoi = (VOI)getImageA().getVOIs().remove(i);
    //    break;
    //}
//}
//VOIVector tempVOI = (VOIVector)getImageA().getVOIs().clone();
//VOIVector zeroVOI = getImageA().getVOIs();  //not cloned to maintain consistency of for loop

//System.out.println("Size: "+getImageA().getVOIs().size());
//int k = zeroVOI.size();
//int j=0;
//int count = 0;


//srcImage.getVOIs().removeAllElements();
//if(removedVoi != null) {
//    srcImage.registerVOI(removedVoi);
//}

//while(count<k*j) {
//    if(!(Boolean)zeroStatus.get(((VOI)zeroVOI.get(j)).getName())) {
//        ((ModelImage)getImageA()).getVOIs().remove(j);
//    } else {
//        j++;
//    }
//    count++;
    //System.out.println("Size: "+getImageA().getVOIs().size());
//}

//BitSet fullMask = getImageA().generateVOIMask();

//for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
//    getImageA().set(i, REMOVED_INTENSITY);
    //componentImage.getImageA().set(i, REMOVED_INTENSITY);
    //srcImage.set(i, REMOVED_INTENSITY);
//}

//srcImage.setMask(fullMask);

//Display VOI of current objects.

//getImageA().getVOIs().removeAllElements();
//srcImage.registerVOI(v);
//need to register it to componentImage
//System.out.println("About to register: "+v.getName());
//componentImage.getImageA().registerVOI(v);
//componentImage.getImageA().clearMask();

//getImageA().clearMask();

//should be process in general, change and compute based on srcImage, do not need to load!
//simply resets the image, otherwise would have those removed intensities
//componentImage.setImageA(srcImage);
