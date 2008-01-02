import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import com.lowagie.text.*;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.pdf.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
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
    
    public static final Color[] colorPick = {Color.BLUE, Color.RED, Color.GREEN, Color.ORANGE, Color.CYAN, 
                                                Color.PINK, Color.YELLOW, Color.MAGENTA, Color.WHITE};
    
    //~ Instance fields ------------------------------------------------------------------------------------------------    
    
    /** denotes the type of srcImg (see enum ImageType) */
    private ImageType imageType; 
    
    /** the parent frame. */
    private Frame parentFrame;
    
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
        writeToPDF();
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
        
        mirrorArr[1] = new String[3];
        mirrorArr[1][0] = "Psoas";
        mirrorArr[1][1] = "Lateral Abdominal";
        mirrorArr[1][2] = "Paraspinous";
        
        boolean[] mirrorZ = {true, true, true};
        
        String[][] noMirrorArr = new String[3][];
        
        noMirrorArr[2] = new String[2];
        noMirrorArr[2][0] = "Aortic calcium";
        noMirrorArr[2][1] = "Rectus abdominus";
        
        boolean[] noMirrorZ = {true, true};
        
        //MuscleImageDisplayTest display = new MuscleImageDisplayTest(((ViewJFrameImage)parentFrame).getImageA(), mirrorArr, mirrorZ, 
        //        noMirrorArr, noMirrorZ, ImageType.ABDOMEN, Symmetry.LEFT_RIGHT);

        
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
        
        MuscleImageDisplay display = new MuscleImageDisplay(((ViewJFrameImage)parentFrame).getImageA(), titles, fillIn, mirrorArr, mirrorZ, 
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
    	
    	private String buttonStringGroup[] = {OK, CLEAR, HELP};
    	
    	private JButton buttonGroup[];
    	
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
            
            for(int i=0; i<buttonStringGroup.length; i++) {
            	buttonGroup[i] = new JButton(buttonStringGroup[i]);
            	buttonGroup[i].addActionListener(parentFrame);
            	buttonGroup[i].addActionListener(this);
            	buttonGroup[i].setActionCommand(buttonStringGroup[i]);
            	buttonGroup[i].setMinimumSize(MipavUtil.defaultButtonSize);
            	buttonGroup[i].setPreferredSize(MipavUtil.defaultButtonSize);
            	buttonGroup[i].setFont(MipavUtil.font12B);
            	
            	buttonPanel.add(buttonGroup[i]);
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
        
        /**Whether this dialog completed. */
        private boolean completed;
        
        /**Whether this dialog produced a novel Voi. */
        private boolean novelVoiProduced;

        public VoiDialogPrompt(MuscleImageDisplay theParentFrame, String objectName, boolean closedVoi, int numVoi, TreeMap zeroStatus) {
            super(theParentFrame);
           
            setButtons(buttonStringList);
            
            this.objectName = objectName;
            this.closedVoi = closedVoi;
            this.numVoi = numVoi;

            
            novelVoiProduced = false;
            completed = false;
            
        }
        
        public void actionPerformed(ActionEvent e) {

            String command = e.getActionCommand();

            if(command.equals(CLEAR)) {
                //clear all VOIs drawn
                parentFrame.getImageA().unregisterAllVOIs();
                parentFrame.updateImages();
            } else {

                if (command.equals("OK")) {
                    VOI goodVoi = checkVoi();
                    //check that VOI conforms to requirements, returns the VOI being modified/created
                    
                    if ( goodVoi != null ) { 
                        
                        //save modified/created VOI to file
                        parentFrame.getImageA().unregisterAllVOIs();
                        parentFrame.getImageA().registerVOI(goodVoi);
                        String dir = parentFrame.getImageA().getImageDirectory()+MuscleImageDisplay.VOI_DIR;
                        parentFrame.saveAllVOIsTo(dir);
                        
                        String fileDir = parentFrame.getImageA().getFileInfo(0).getFileDirectory();

                        MipavUtil.displayInfo(objectName+" VOI saved in folder\n " + fileDir + MuscleImageDisplay.VOI_DIR);
                        
                        parentFrame.getImageA().unregisterAllVOIs();
                        parentFrame.updateImages();
                        
                        completed = true;
                        novelVoiProduced = true; //not necessarily
                        notifyListeners(OK);
                        //dispose();
                    } else {
                        //individual error messages are already displayed
                    }
                } else if (command.equals("Cancel")) {
                    setCompleted(savedVoiExists());
                    notifyListeners(CANCEL);
                    //dispose();
                } else if (command.equals("Help")) {
                    MipavUtil.showHelp("19014");
                }
            }
            
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
        
        private void setCompleted(boolean completed) {
            this.completed = completed;
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
            
            String voiStr = new String("Select "+numVoi+" "+closedStr+"VOI curve"+pluralVOI+" around the "+
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
            int qualifiedVoi = 0;
            VOIVector srcVOI = ((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).getVOIs();
            int countQualifiedVOIs = 0; //equal to 1 when the right  amount of VOIs have been created
            VOI goodVOI = null;
            for(int i=0; i<srcVOI.size(); i++) {
                //System.out.println(((VOI)srcVOI.get(i)).getName());
                if(((VOI)srcVOI.get(i)).getName().equals(objectName)) {
                    goodVOI = ((VOI)srcVOI.get(i));
                    countQualifiedVOIs++;
                    qualifiedVoi = 1;
                }
            }
            
            
            //for(int i=0; i<srcVOI.size(); i++) {
            //    String cmp = ((VOI)srcVOI.get(i)).getName();
            //    if(zeroStatus.get(cmp) == null) {
            //        qualifiedVoi++;
            //        goodVOI = (VOI)srcVOI.get(i);
            //    }
            //}
            
            if(qualifiedVoi != 1) {
                String error = qualifiedVoi > 1 ? "You have created too many VOIs." : 
                                                                "You haven't created any VOIs.";
                MipavUtil.displayError(error);
                return null;  
            }
            Vector[] curves = goodVOI.getCurves();
            VOI voi = goodVOI;
            if(((Vector)curves[0]).size() == numVoi) {
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
                String error = ((Vector)curves[0]).size() > numVoi ? "You have created too many curves." : 
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
        
    	private AnalysisPrompt analysisPrompt;

        public static final int REMOVED_INTENSITY = -2048;
        
        public static final String CHECK_VOI = "CHECK_VOI";
        
        public static final String VOI_DIR = "NIA_Seg\\";
        
        private VoiDialogPrompt voiPrompt;
        
        private int voiIndex;
        
        private int componentShown = 0;

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
        
        private MuscleDialogPrompt[] tabs;
        
        private TreeMap zeroStatus;
        
        private String[] titles; 
        
        //does have zeroes
        private ModelImage tempImage2;
        
        private boolean[] fillIn;
        
        private TreeMap fillStatus;
        
        private TreeMap locationStatus;
        
        private ModelImage[] imageDiff;
        
        private boolean componentRock = false;
        
        private BuildThighAxes thighAxes;
        
        private boolean stateChanged;
        
        public MuscleImageDisplay(ModelImage image, String[] titles, boolean[] fillIn,
                String[][] mirrorArr, boolean[][] mirrorZ, 
                String[][] noMirrorArr, boolean[][] noMirrorZ,  
                ImageType imageType, Symmetry symmetry) {
            
            
            super(image);
            //super(image, true);
            //if we don't have an image, then we're done
            this.setImageA(image);
            tempImage2 = (ModelImage)image.clone();
            this.fillIn = fillIn;
            this.titles = titles;
            this.mirrorArr = mirrorArr;
            this.mirrorZ = mirrorZ;
            this.noMirrorArr = noMirrorArr;
            this.noMirrorZ = noMirrorZ;
            this.imageType = imageType;
            this.symmetry = symmetry;
            this.imageDiff = new ModelImage[fillIn.length];
            for(int i=0; i<fillIn.length; i++) {
                imageDiff[i] = (ModelImage)image.clone();
            }
            
            voiIndex = fillIn.length;
            
            fillStatus = new TreeMap();
            locationStatus = new TreeMap();
            
            stateChanged = false;
            
            if (imageA == null) {
                return;
            }
            
            initNext();

        }
        
        /**
         * This method saves all VOIs for the active image to a given directory.  Note:  This method differs quite a bit in execution 
         * compared with saveALLVOIsTo(voiDir) in ViewJFrameBase (its direct super method).
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
                voiPrompt = new VoiDialogPrompt(this, voiString, true, 1, zeroStatus);
                // This is very important. Adding this object as a listener allows the subdialog to
                // notify this object when it has completed or failed. 
                // This is could be generalized by making a subDialog interface.
                voiPrompt.addListener(this);
                voiPrompt.addComponentListener(this);
                lockToPanel(voiPrompt, "VOI");
                //setVisible(false); // Hide dialog
            } else if(command.equals(DialogPrompt.CALCULATE)) {
            	//display the result display prompt in same way
            	System.out.println("OK Command, proceed with analysis.");
            	analysisPrompt = new AnalysisPrompt(this, mirrorArr, noMirrorArr, symmetry);
            	// This is could be generalized by making a subDialog interface.
            	analysisPrompt.addListener(this);
            	analysisPrompt.addComponentListener(this);
            	lockToPanel(analysisPrompt, "Analysis");
            	
            } else {/*if(command.equals(VoiDialogPrompt.SUB_DIALOG_COMPLETED*/
            	unlockToPanel();
                initMuscleImage(activeTab);
            }
        }
        
        private JPanel initDialog() {
//          The component image will be displayed in a scrollpane.
            zeroStatus = new TreeMap();
            
            scrollPane = new JScrollPane(componentImage, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                         JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

            imagePane = new JTabbedPane();
            
            tabs = new MuscleDialogPrompt[mirrorArr.length];

            for(int i=0; i<tabs.length; i++) {
                tabs[i] = new MuscleDialogPrompt(this, mirrorArr[i], mirrorZ[i],
                                                    noMirrorArr[i], noMirrorZ[i],
                                                    imageType, symmetry);
                
                tabs[i].addComponentListener(this);
                tabs[i].addListener(this);
                tabs[i].setName(titles[i]);
                imagePane.addTab((i+1)+": "+titles[i], tabs[i]);
                
                zeroStatus.putAll(tabs[i].getZeroStatus());
                fillStatus.put(titles[i], fillIn[i]);
                
                                                                            
            }
            
            JPanel panelA = new JPanel(new GridLayout(1, 3, 10, 10));
            
            splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, imagePane, scrollPane);
            scrollPane.setBackground(Color.black);
            panelA.add(splitPane);
            
            for(int i=0; i<tabs.length; i++) {
                JButton[] mirror = tabs[i].getMirrorButton();
                JButton[] noMirror = tabs[i].getNoMirrorButton();
                for(int j=0; j<mirror.length; j++) {
                    locationStatus.put(mirror[j].getText(), i);
                }
                for(int j=0; j<noMirror.length; j++) {
                    locationStatus.put(noMirror[j].getText(), i);
                }
            }
            
            return panelA;
            
        }
        
        private void initNext() {
            JPanel panelA = initDialog();
            
            getContentPane().add(panelA);
            getContentPane().remove(0);
            Container c = getContentPane();
            
            pack();
            initMuscleImage(0);
            this.setResizable(false);
        }
        
        @Override
		public void componentHidden(ComponentEvent event) {
		    System.out.println("Selected pane: "+imagePane.getSelectedIndex()+"\tVOI pane: "+voiIndex);
		    System.out.println("Active pane: "+activeTab);
		    Component c = event.getComponent();
		    Object obj = event.getSource();
		    if(imagePane != null) {
		        if(event.getComponent().equals(tabs[activeTab])) {
		            getImageA().unregisterAllVOIs();
		            updateImages(true);
		        }
		        if(c instanceof MuscleDialogPrompt) {
		            if(imagePane.getSelectedIndex() == voiIndex) {
		                getImageA().unregisterAllVOIs();
		                initVoiImage(voiPrompt, activeTab);
		                updateImages(true);
		            }
		        }
		    }
		    super.componentHidden(event);
		}

		@Override
		public void componentShown(ComponentEvent event) {
		    System.out.println("Active pane: "+activeTab);
		    //Component c = event.getComponent();
		    //Object obj = event.getSource();
		    if(imagePane != null) {
		        boolean found = false;
		        for(int i=0; i<tabs.length; i++) {
		            if(!found) {
		                if(event.getComponent().equals(tabs[i])) {
		                    initMuscleImage(i);
		                    activeTab =  i;
		                    found = true; 
		                }
		            }
		        }
		    }
		    super.componentShown(event);
		}

		/**
         * Resizes frame and all components.
         *
         * @param  event  event that triggered function
         */
        public synchronized void componentResized(ComponentEvent event) {
            
            //System.out.println("Resizing here.");
            
        }
        
        public void lockToPanel(JPanel panel, String title) {
            //System.out.println("Got here.");
            activeTab = imagePane.getSelectedIndex();
            for(int i=0; i<imagePane.getTabCount(); i++) {
                imagePane.setEnabledAt(i, false);
            }
            imagePane.addTab(title, panel);
            imagePane.setSelectedIndex(imagePane.getTabCount()-1);
   
        }
        
        public void unlockToPanel() {
            imagePane.removeTabAt(imagePane.getTabCount()-1);
            for(int i=0; i<imagePane.getTabCount(); i++) {
                imagePane.setEnabledAt(i, true);
            }
            imagePane.setSelectedIndex(activeTab);
            
        }
        
        
        //TODO: Local copies of VOIs
        
        private void loadVOI(int pane) {
            getImageA().unregisterAllVOIs();
            int colorChoice = 0;
            String fileDir = getImageA().getFileInfo(0).getFileDirectory()+VOI_DIR;
            File allVOIs = new File(fileDir);
            if(allVOIs.isDirectory()) {
                String[] voiName = allVOIs.list();
                for(int i=0; i<voiName.length; i++) {
                    System.out.println(voiName[i]);

                    if(new File(fileDir+voiName[i]).exists()) {
                        String fileName = voiName[i];
                        FileVOI v;
                        VOI[] voiVec = null;
                        try {
                            v = new FileVOI(fileName, fileDir, getImageA());
                            voiVec = v.readVOI(false);
                        } catch(IOException e) {
                            MipavUtil.displayError("Unable to load old VOI from location:\n"+fileDir+"\nWith name: "+fileName);
                        }
                        if(voiVec.length > 1) {
                            MipavUtil.displayError("Invalid VOI from location:\n"+fileDir+"\nWith name: "+fileName);
                        } else {               
                            if(pane == -1 || (Integer)locationStatus.get(voiName[i].substring(0, voiName[i].indexOf(".xml"))) == pane) {
                                getImageA().registerVOI(voiVec[0]);
                            }
                        }
                    }
                }
            }
        }
        
        private void initVOIColor() {
            int colorChoice = 0;
            VOIVector voiFullVec = getImageA().getVOIs();
            for(int i=0; i<voiFullVec.size(); i++) {
                Color c = hasColor((VOI)voiFullVec.get(i));
                if(c != null) {
                    ((VOI)voiFullVec.get(i)).setColor(c);
                    
                } else {
                    ((VOI)voiFullVec.get(i)).setColor(colorPick[colorChoice % colorPick.length]);
                    colorChoice++;
                }
                //getImageA().registerVOI((VOI)voiFullVec.get(i));
            }
        }
        
        private void initMuscleImage(int pane) {
            
            
            ///if pane == -1, will load all VOIs
            loadVOI(pane);
            initVOIColor();
            
            ctMode(getImageA(), -175, 275);
            
            
            //if(thighAxes == null || stateChanged){
            //    thighAxes = new BuildThighAxes(getImageA(), 0);
            //    thighAxes.createAxes();
            //} //else they're loaded in loadVOI
            //added before button check so that they can be accessed in this way, optional to change.
            
            VOIVector vec = getImageA().getVOIs();
            if(pane != -1) {
            	for(int i=0; i<vec.size(); i++) {            
            		for(int j=0; j<tabs.length; j++) {
            			if(tabs[j] instanceof MuscleDialogPrompt) {
            				if(((MuscleDialogPrompt)tabs[j]).hasButton(((VOI)vec.get(i)).getName())) {
            					((MuscleDialogPrompt)tabs[j]).setButton(((VOI)vec.get(i)).getName());
            				}	
            			}
            		}
            	}
            } else {
            	//working with results tab
            	System.out.println("The results tab follows here.");
            }
            
            
            
            updateImages(true);
            //componentRock = true;
        }
        
        
        //TODO: local copies of VOIs, combine with loadVOI(int pane)
        
        
        private void initVoiImage(VoiDialogPrompt voiDialog, int pane) {
            //getImageA().unregisterAllVOIs();
            componentRock = true;
            int colorChoice = 0;
            String fileDir = getImageA().getFileInfo(0).getFileDirectory()+VOI_DIR;
            File allVOIs = new File(fileDir);
            if(allVOIs.isDirectory()) {
                String[] voiName = allVOIs.list();
                for(int i=0; i<voiName.length; i++) {
                    System.out.println(voiName);
                    
                    if(new File(fileDir+voiName[i]).exists()) {
                        String fileName = voiName[i];
                        FileVOI v;
                        VOI[] voiVec = null;
                        try {
                            v = new FileVOI(fileName, fileDir, getImageA());
                            voiVec = v.readVOI(false);
                        } catch(IOException e) {
                        	//though could be abdomen (not currently)
                            MipavUtil.displayError("Unable to load old VOI from location:\n"+fileDir+"\\NIA_Seg\\Thigh_VOI\\"+"\nWith name: "+fileName);
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
                            if((Integer)locationStatus.get(voiName[i].substring(0, voiName[i].indexOf(".xml"))) == pane) {
                                getImageA().registerVOI(voiVec[0]);
                            }
                            
                        }
                    }
                }
            }
            
            componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
            
            setImageA(tempImage2);
            srcImage = (ModelImage)getImageA().clone();
            
            VOIVector vector = srcImage.getVOIs();
            VOI removedVoi = null;
            
            for(int i=0; i < vector.size() ; i++) {
            	//TODO: debug
            	//Find same voi, and remove it from original image
                //if(((VOI)vector.get(i)).getName().equals(voiDialog.getObjectName())) {
                //    removedVoi = (VOI)getImageA().getVOIs().remove(i);
                //    break;
                //}
            }
            VOIVector tempVOI = (VOIVector)getImageA().getVOIs().clone();
            VOIVector zeroVOI = getImageA().getVOIs();  //not cloned to maintain consistency of for loop
            
            //System.out.println("Size: "+getImageA().getVOIs().size());
            int k = zeroVOI.size();
            int j=0;
            int count = 0;
            
            
            srcImage.getVOIs().removeAllElements();
            if(removedVoi != null) {
                srcImage.registerVOI(removedVoi);
            }
            
            while(count<k*j) {
                if(!(Boolean)zeroStatus.get(((VOI)zeroVOI.get(j)).getName())) {
                    ((ModelImage)getImageA()).getVOIs().remove(j);
                } else {
                    j++;
                }
                count++;
                //System.out.println("Size: "+getImageA().getVOIs().size());
            }
            
            BitSet fullMask = getImageA().generateVOIMask();

            for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
                getImageA().set(i, REMOVED_INTENSITY);
                //componentImage.getImageA().set(i, REMOVED_INTENSITY);
                //srcImage.set(i, REMOVED_INTENSITY);
            }
            
            srcImage.setMask(fullMask);
            
            //Display VOI of current objects.
            
            getImageA().getVOIs().removeAllElements();
            
            for(int i=0; i<tempVOI.size(); i++) {
                VOI v = (VOI)tempVOI.get(i);
                if((Boolean)zeroStatus.get(v.getName())) {
                    v.setDisplayMode(VOI.SOLID);
                } else {
                    v.setDisplayMode(VOI.CONTOUR);
                }
                //srcImage.registerVOI(v);
                //need to register it to componentImage
                System.out.println("About to register: "+v.getName());
                componentImage.getImageA().registerVOI(v);
               
            }
            componentImage.getImageA().clearMask();
            
            getImageA().clearMask();
            
            //should be process in general, change and compute based on srcImage, do not need to load!
            //simply resets the image, otherwise would have those removed intensities
            componentImage.setImageA(srcImage);
            updateImages(true);
            //LOOK HERE
        }

		private Color hasColor(VOI voiVec) {
            Color c = null;
            VOIVector tempVec = componentImage.getImageA().getVOIs();
            for(int i=0; i<tempVec.size(); i++) {
                if(voiVec.getName().contains("Left") || voiVec.getName().contains("Right")) {
                    if( !(((VOI)tempVec.get(i)).getName().contains("Left")  &&  voiVec.getName().contains("Left")) && 
                            !(((VOI)tempVec.get(i)).getName().contains("Right")  &&  voiVec.getName().contains("Right")) && 
                            ((VOI)tempVec.get(i)).getName().endsWith(voiVec.getName().substring(voiVec.getName().indexOf(" ")))) {
                        c =  ((VOI)tempVec.get(i)).getColor();
                    }
                }
            }
            return c;
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
                int nVOI, nContours;
                float[] xPoints = null;
                float[] yPoints = null;
            
                VOIs = image.getVOIs(); //note that VOIs must already be loaded
            
                nVOI = VOIs.size();
            
                if (nVOI == 0) {
                    return;
                }
                
                thighVOIs = new VOI[2];
                
                for (groupNum = 0; groupNum < nVOI; groupNum++) {
            
                    if (((VOI)VOIs.get(groupNum)).getName().equals("Left Thigh")) {
                        thighVOIs[0] = ((VOI)VOIs.get(groupNum));
                    }
                    else if (((VOI)VOIs.get(groupNum)).getName().equals("Right Thigh")) {
                        thighVOIs[1] = ((VOI)VOIs.get(groupNum));
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
                    nContours = contours[i][zSlice].size();
            
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
            } else {
                return false;
            }
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
                    if(((VOI)existingVois.get(j)).getName().equals(mirrorButtonArr[i].getText())) {
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
                
                System.out.println(mirrorButtonArr[i].getText()+" is "+mirrorZ[i/2]);
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
                    if(((VOI)existingVois.get(j)).getName().equals(noMirrorButtonArr[i].getText())) {
                        noMirrorCheckArr[i].setSelected(true);
                    }
                }
                
                System.out.println(noMirrorButtonArr[i].getText()+" is "+noMirrorZ[i]);
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
		

		
		private final String[] buttonStringList = {HELP, EXIT};
		
		boolean novelVoiProduced;
		
		boolean completed;
	
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
		private Vector objectList = new Vector();
		
		private String name[] = {"thigh", "bone component", "muscle"};
		
		public AnalysisPrompt(MuscleImageDisplay theParentFrame, String[][] mirrorArr, String[][] noMirrorArr, Symmetry symmetry) {
	        super(theParentFrame);
	        
	        setButtons(buttonStringList);
	        
	        this.noMirrorArr = noMirrorArr;
	        this.mirrorArr = mirrorArr;
	        
	        this.symmetry = symmetry;
	        
	        //this.zeroStatus = zeroStatus;
	        
	        novelVoiProduced = false;
	        completed = false;
	        
	        //initImage();
	        initDialog();
	        
	        
	        
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
	        
	        VOIVector existingVois = ((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).getVOIs();
	         
	        //JCheckBox[] mirrorCheckArr = new JCheckBox[mirrorArr[index].length * 2];
	        String[] mirrorString = new String[mirrorArr[index].length * 2];
	        ButtonGroup mirrorGroup = new ButtonGroup();
	       
	        
	        for(int i=0; i<mirrorArr[index].length * 2; i++) {
	            String symmetry1 = "", symmetry2 = "";
	            if(symmetry.equals(Symmetry.LEFT_RIGHT)) {
	                symmetry1 = "Left ";
	                symmetry2 = "Right ";
	            } else if(symmetry.equals(Symmetry.TOP_BOTTOM)) {
	                symmetry1 = "Top ";
	                symmetry2 = "Bottom ";
	            }
	            //mirrorCheckArr[i] = new JCheckBox();
	            //mirrorCheckArr[i].setEnabled(false);
	            //mirrorCheckArr[i].setHorizontalAlignment(SwingConstants.RIGHT);
	            
	            
	            mirrorString[i] = (i % 2) == 0 ? new String(symmetry1+mirrorArr[index][i/2]) : 
	                                                        new String(symmetry2+mirrorArr[index][i/2]);
	            //mirrorString[i].setFont(MipavUtil.font12B);
	            //mirrorString[i].setActionCommand(MuscleImageDisplay.CHECK_VOI);
	            //mirrorString[i].addActionListener(parentFrame);
	            //mirrorGroup.add(mirrorString[i]);
	            
	            //if(i != 0 && i % 4 == 0) {
	            //    gbc.gridy++;
	            //    gbc.gridx = 0;
	            //}
	            //gbc.weightx = 0;
	            //mirrorPanel.add(mirrorCheckArr[i], gbc);
	            //gbc.gridx++;
	            //gbc.weightx = 1;
	            //mirrorPanel.add(mirrorString[i], gbc);
	            //gbc.gridx++;
	            
	            //System.out.println(mirrorButtonArr[i].getText()+" is "+mirrorZ[i/2]);
	            //zeroStatus.put(mirrorButtonArr[i].getText(), mirrorZ[i/2]);
	        
	        }
	        
	        list[index] = new JList(mirrorString);
	        list[index].setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
	        list[index].setLayoutOrientation(JList.HORIZONTAL_WRAP);
	        list[index].setVisibleRowCount(mirrorArr[index].length); //was*2
	        list[index].addListSelectionListener(this);
	        
	        JScrollPane mirrorPanel = new JScrollPane(list[index], JScrollPane.VERTICAL_SCROLLBAR_NEVER,
	        											JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
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
	                    
	                    completed = true;
	                    novelVoiProduced = true; //not necessarily
	                    notifyListeners(OK);
	                    //dispose();
	                } else {
	                    //individual error messages are already displayed
	                }
	            } else if (command.equals("Cancel")) {
	         
	                setCompleted(false);//savedVoiExists());
	                notifyListeners(CANCEL);
	                //dispose();
	            } else if (command.equals("Help")) {
	                MipavUtil.showHelp("19014");
	            }
	        }
	        
	    }
	    
	
	    
	    public void valueChanged(ListSelectionEvent e) {
	
	    	//should be process in general, change and compute based on srcImage, do not need to load into component,
	    	//though that's where VOIs should be registered.

	    	//System.out.println("Number of VOIs at outset: "+parentFrame.getActiveImage().getVOIs().size());
	    	//System.out.println("Number of VOIs at outset: "+parentFrame.getImageA().getVOIs().size());
	    	
	    	//parentFrame.getActiveImage().unregisterAllVOIs();
	    	//parentFrame.updateImages(true);
	    	//System.out.println("Number of VOIs at outset: "+parentFrame.getActiveImage().getVOIs().size());
	    	JList source = (JList)e.getSource();
	    	Object[] selected = source.getSelectedValues();
	    	String[] selectedString = new String[selected.length];
	    	for(int i=0; i<selected.length; i++) {
	    		selectedString[i] = (String)selected[i]+".xml";
	    		System.out.println("Selected "+i+": "+selectedString[i]);
	    		
	    	}
	    	//Load VOIs and calculations
	    	loadVOIs(selectedString);
	    	
	    	
	    }
	    
	    private void loadVOIs(String[] voiName) {
            parentFrame.getActiveImage().unregisterAllVOIs();
            int colorChoice = 0;
            String fileDir = parentFrame.getActiveImage().getFileInfo(0).getFileDirectory()+parentFrame.VOI_DIR;
            File allVOIs = new File(fileDir);
            if(allVOIs.isDirectory()) {
                for(int i=0; i<voiName.length; i++) {
                    System.out.println(voiName[i]);

                    if(new File(fileDir+voiName[i]).exists()) {
                        String fileName = voiName[i];
                        FileVOI v;
                        VOI[] voiVec = null;
                        try {
                            v = new FileVOI(fileName, fileDir, parentFrame.getActiveImage());
                            voiVec = v.readVOI(false);
                        } catch(IOException e) {
                            MipavUtil.displayError("Unable to load old VOI from location:\n"+fileDir+"\nWith name: "+fileName);
                        }
                        if(voiVec.length > 1) {
                            MipavUtil.displayError("Invalid VOI from location:\n"+fileDir+"\nWith name: "+fileName);
                        } else {      
                        	parentFrame.getActiveImage().registerVOI(voiVec[0]);
                        }
                    }
                }
            }
            parentFrame.updateImages(true);
	    }
	}
    
	private void writeToPDF() {
		JFileChooser chooser = new JFileChooser();
		chooser.setDialogTitle("Save to PDF");
		chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { "pdf" }));
		
		File file = null;
		int returnVal = chooser.showSaveDialog(null);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
           file = chooser.getSelectedFile();
        } else {
        	return;
        }
		
		try {
			Document pdfDocument = new Document();
			PdfWriter writer = PdfWriter.getInstance(pdfDocument, new FileOutputStream(file));
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
			mct2.addElement(new Paragraph("1000140811", fontNormal));
			mct2.addElement(new Paragraph("Center:", fontBold));
			mct2.addElement(new Paragraph("Hjartavernd", fontNormal));
			pdfDocument.add(mct2);
			
			MultiColumnText mct3 = new MultiColumnText(20);
			mct3.setAlignment(Element.ALIGN_LEFT);
			mct3.addRegularColumns(pdfDocument.left(), pdfDocument.right(), 10f, 4);
			mct3.addElement(new Paragraph("ID:", fontBold));
			mct3.addElement(new Paragraph("1000140811", fontNormal));
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
			spTable.addCell("0.976562");
			spTable.addCell("Slice Thickness: (mm)");
			spTable.addCell("10.00");
			spTable.addCell("Table Height: (cm)");
			spTable.addCell("143.00");
			
			Paragraph pTable = new Paragraph();
			pTable.add(new Paragraph());
			pTable.setAlignment(Element.ALIGN_CENTER);
			pTable.add(spTable);
			pdfDocument.add(new Paragraph(new Chunk("")));
			pdfDocument.add(new Paragraph(pTable));
			
			pdfDocument.add(new Paragraph(new Chunk("")));
			
			PdfPTable aTable = new PdfPTable(7);
			
			// add Column Titles (in bold)
			aTable.addCell(new PdfPCell(new Paragraph("Area (cm^2)", new Font(Font.TIMES_ROMAN, 13, Font.BOLD))));
			aTable.addCell(new PdfPCell(new Paragraph("Total Area", new Font(Font.TIMES_ROMAN, 13, Font.BOLD))));
			aTable.addCell(new PdfPCell(new Paragraph("Fat Area", new Font(Font.TIMES_ROMAN, 13, Font.BOLD))));
			aTable.addCell(new PdfPCell(new Paragraph("Lean Area", new Font(Font.TIMES_ROMAN, 13, Font.BOLD))));
			aTable.addCell(new PdfPCell(new Paragraph("Fat HU", new Font(Font.TIMES_ROMAN, 13, Font.BOLD))));
			aTable.addCell(new PdfPCell(new Paragraph("Lean HU", new Font(Font.TIMES_ROMAN, 13, Font.BOLD))));
			aTable.addCell(new PdfPCell(new Paragraph("Total HU", new Font(Font.TIMES_ROMAN, 13, Font.BOLD))));
			
			//start adding columns
			int numRows = 5;
			for (int i = 0; i < numRows; i++) {
				//name of area
				aTable.addCell("L. Thigh Total");
				
				//total area
				aTable.addCell("104.78");
				
				//fat area
				aTable.addCell("6.11");
			
				//lean area
				aTable.addCell("98.58");
				
				//fat HU
				aTable.addCell("-66.13");
				
				//lean HU
				aTable.addCell("41.90");
				
				//total HU
				aTable.addCell("35.53");
			}
			
			Paragraph aPar = new Paragraph();
			aPar.setAlignment(Element.ALIGN_CENTER);
			aPar.add(new Paragraph());
			aPar.add(aTable);
			pdfDocument.add(new Paragraph());
			pdfDocument.add(aPar);
		
			
			PdfPTable imageTable = new PdfPTable(2);
			imageTable.addCell("Edge Image");
			imageTable.addCell("QA Image");
			
			chooser = new JFileChooser();
			chooser.setDialogTitle("Open image 1");
			returnVal = chooser.showOpenDialog(null);

	        while (returnVal != JFileChooser.APPROVE_OPTION) {
	        	returnVal = chooser.showOpenDialog(null);
	        } 
			
			imageTable.addCell(Image.getInstance(chooser.getSelectedFile().getPath()));
			
			chooser.setDialogTitle("Open image 1");
			returnVal = chooser.showOpenDialog(null);

	        while (returnVal != JFileChooser.APPROVE_OPTION) {
	        	returnVal = chooser.showOpenDialog(null);
	        } 
			
	        imageTable.addCell(Image.getInstance(chooser.getSelectedFile().getPath()));
	        
	        Paragraph pImage = new Paragraph();
	        pImage.add(new Paragraph());
	        pImage.add(imageTable);
	        
			pdfDocument.add(pImage);
			pdfDocument.close();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
    
}
