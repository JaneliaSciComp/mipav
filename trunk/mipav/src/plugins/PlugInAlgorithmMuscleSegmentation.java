import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileVOI;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.IOException;
import java.util.*;

import javax.swing.*;


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
        
        MuscleImageDisplayTest display = new MuscleImageDisplayTest(((ViewJFrameImage)parentFrame).getImageA(), titles, fillIn, mirrorArr, mirrorZ, 
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

    private class VoiDialogPrompt extends JPanel implements ActionListener {
        
        //~ Static fields/initializers -------------------------------------------------------------------------------------

        public static final String SUB_DIALOG_COMPLETED = "SUB_DIALOG_COMPLETED";
        
        public static final String CLEAR = "Clear";
        
        private String objectName;
        
        private boolean closedVoi;
        
        private int numVoi;
        
        /**Whether this dialog completed. */
        private boolean completed;
        
        /**Whether this dialog produced a novel Voi. */
        private boolean novelVoiProduced;
        
        private JButton clearButton;
        
        /**
         * Vector list of objects that listen to this dialog box. When the action for this pseudo-algorithm has
         * completed, the program will notify all listeners.
         */
        private Vector objectList = new Vector();
        
        //private MuscleDialogPrompt muscle;
        
        private JButton OKButton;
        
        private JButton cancelButton;
        
        private MuscleImageDisplayTest parentFrame;
        
        private TreeMap zeroStatus;
        
        
        public VoiDialogPrompt(MuscleImageDisplayTest theParentFrame, String objectName, boolean closedVoi, int numVoi, TreeMap zeroStatus) {
            super();
            
            this.parentFrame = theParentFrame;
            
            this.objectName = objectName;
            this.closedVoi = closedVoi;
            this.numVoi = numVoi;
            this.zeroStatus = zeroStatus;
            
            //this.zeroStatus = zeroStatus;
            
            novelVoiProduced = false;
            completed = false;
            
            //initImage();
            initDialog();
            
        }
        
        public void actionPerformed(ActionEvent e) {
            String command = e.getActionCommand();
            if(command.equals(CLEAR)) {
                //clear all VOIs drawn
                ((MuscleImageDisplayTest)parentFrame).getImageA().unregisterAllVOIs();
                ((MuscleImageDisplayTest)parentFrame).updateImages();
            } else {

                if (command.equals("OK")) {
                    VOI goodVoi = checkVoi();
                    //check that VOI conforms to requirements, returns the VOI being modified/created
                    
                    if ( goodVoi != null ) { 
                        
                        //save modified/created VOI to file
                        ((MuscleImageDisplayTest)parentFrame).getImageA().unregisterAllVOIs();
                        ((MuscleImageDisplayTest)parentFrame).getImageA().registerVOI(goodVoi);
                        String dir = ((MuscleImageDisplayTest)parentFrame).getImageA().getImageDirectory()+MuscleImageDisplayTest.VOI_DIR;
                        ((MuscleImageDisplayTest)parentFrame).saveAllVOIsTo(dir);
                        
                        String fileDir = ((MuscleImageDisplayTest)parentFrame).getImageA().getFileInfo(0).getFileDirectory();

                        MipavUtil.displayInfo(objectName+" VOI saved in folder\n " + fileDir + MuscleImageDisplayTest.VOI_DIR);
                        
                        ((MuscleImageDisplayTest)parentFrame).getImageA().unregisterAllVOIs();
                        ((MuscleImageDisplayTest)parentFrame).updateImages();
                        
                        completed = true;
                        novelVoiProduced = true; //not necessarily
                        notifyListeners();
                        //dispose();
                    } else {
                        //individual error messages are already displayed
                    }
                } else if (command.equals("Cancel")) {
                    setCompleted(savedVoiExists());
                    notifyListeners();
                    //dispose();
                } else if (command.equals("Help")) {
                    MipavUtil.showHelp("19014");
                }
            }
            
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
        public void notifyListeners() {

            for (int i = 0; i < objectList.size(); i++) {
                ((ActionListener) objectList.elementAt(i)).actionPerformed(new ActionEvent(this, 0, SUB_DIALOG_COMPLETED));
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
            String fileName = new String(((MuscleImageDisplayTest)parentFrame).getImageA().getFileInfo(0).getFileDirectory()+MuscleImageDisplayTest.VOI_DIR+objectName+".xml");
            return new File(fileName).exists();
        }
        
        private void setCompleted(boolean completed) {
            this.completed = completed;
        }
        
        /**
         * Initializes the dialog box.
         *
         */
        
        private void initDialog() {
            setForeground(Color.black);
            addNotify();
            //setTitle("VOI selection");
                        
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
                                        objectName.toLowerCase()+".  When done press \"OK\".");
            JLabel label = new JLabel(voiStr);
            
            label.setFont(MipavUtil.font12);
            mainPanel.add(label, BorderLayout.NORTH);
            //gbc.gridx++;
            
            
            add(mainPanel, BorderLayout.NORTH);
            gbc.gridy++;
            add(buildButtons(), BorderLayout.CENTER);
            //pack();
            //setResizable(false);
            //setVisible(true);
        }
        
        /**
         * Builds button panel consisting of OK, Cancel and Help buttons.
         *
         * @return  JPanel that has ok, cancel, and help buttons
         */
        protected JPanel buildButtons() {
            JPanel buttonPanel = new JPanel();
            
            cancelButton = new JButton("Cancel");
            cancelButton.addActionListener(this);

            // cancelButton.setToolTipText("Cancel action.");
            cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
            cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
            cancelButton.setFont(MipavUtil.font12B);
            
            // size and place the clear buttton
            clearButton = new JButton("Clear");
            clearButton.addActionListener(this);
            clearButton.setActionCommand(CLEAR);
            clearButton.setToolTipText("Clear the VOI.");
            clearButton.setMinimumSize(MipavUtil.defaultButtonSize);
            clearButton.setPreferredSize(MipavUtil.defaultButtonSize);
            clearButton.setFont(MipavUtil.font12B);
            
            OKButton = new JButton("OK");
            OKButton.addActionListener(this);

            // OKButton.setToolTipText("Accept values and perform action.");
            OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
            OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
            OKButton.setFont(MipavUtil.font12B);
            
            buttonPanel.add(OKButton);
            buttonPanel.add(clearButton);
            buttonPanel.add(cancelButton);

            return buttonPanel;
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
            
            
            for(int i=0; i<srcVOI.size(); i++) {
                String cmp = ((VOI)srcVOI.get(i)).getName();
                if(zeroStatus.get(cmp) == null) {
                    qualifiedVoi++;
                    goodVOI = (VOI)srcVOI.get(i);
                }
            }
            
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
    
    public class MuscleImageDisplayTest extends ViewJFrameImage {
        
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
                if(imagePane.getSelectedIndex() == voiIndex) {
                    getImageA().unregisterAllVOIs();
                    initVoiImage(voiPrompt, activeTab);
                    updateImages(true);
                }
            }
            super.componentHidden(event);
        }
        
        @Override
        public void componentShown(ComponentEvent event) {
            System.out.println("Active pane: "+activeTab);
            Component c = event.getComponent();
            Object obj = event.getSource();
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
        
        public MuscleImageDisplayTest(ModelImage image, String[] titles, boolean[] fillIn,
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
                    VOIs = (ViewVOIVector) currentImage.getVOIs();
                } else if (displayMode == IMAGE_B) {
                    currentImage = componentImage.getImageB();
                    VOIs = (ViewVOIVector) currentImage.getVOIs();
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
        
        public void actionPerformed(ActionEvent e) {
            super.actionPerformed(e);
            String command = e.getActionCommand();
            //System.out.println("Caught2: "+command);
            if(command.equals(MuscleImageDisplayTest.CHECK_VOI)) {
                //initVoiImage();
                String voiString = ((JButton)(e.getSource())).getText();
                voiPrompt = new VoiDialogPrompt(this, voiString, true, 1, zeroStatus);
                // This is very important. Adding this object as a listener allows the subdialog to
                // notify this object when it has completed or failed. 
                // This is could be generalized by making a subDialog interface.
                voiPrompt.addListener(this);
                voiPrompt.addComponentListener(this);
                lockToVOI(voiPrompt);
                //setVisible(false); // Hide dialog
            } else if(command.equals(VoiDialogPrompt.SUB_DIALOG_COMPLETED)) {
                unlockToVOI();
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
        }
        
        /**
         * Resizes frame and all components.
         *
         * @param  event  event that triggered function
         */
        public synchronized void componentResized(ComponentEvent event) {
            
            //System.out.println("Resizing here.");
            
        }
        
        public void lockToVOI(VoiDialogPrompt voi) {
            //System.out.println("Got here.");
            activeTab = imagePane.getSelectedIndex();
            for(int i=0; i<imagePane.getTabCount(); i++) {
                imagePane.setEnabledAt(i, false);
            }
            imagePane.addTab("VOI", voi);
            imagePane.setSelectedIndex(imagePane.getTabCount()-1);
   
        }
        
        public void unlockToVOI() {
            imagePane.removeTabAt(imagePane.getTabCount()-1);
            for(int i=0; i<imagePane.getTabCount(); i++) {
                imagePane.setEnabledAt(i, true);
            }
            imagePane.setSelectedIndex(activeTab);
            
        }
        
        
        private void loadVOI(int pane) {
            getImageA().unregisterAllVOIs();
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
                            MipavUtil.displayError("Unable to load old VOI from location:\n"+fileDir+"\nWith name: "+fileName);
                        }
                        if(voiVec.length > 1) {
                            MipavUtil.displayError("Invalid VOI from location:\n"+fileDir+"\nWith name: "+fileName);
                        } else {               
                            if((Integer)locationStatus.get(voiName[i].substring(0, voiName[i].indexOf(".xml"))) == pane) {
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
            
            
            
            loadVOI(pane);
            initVOIColor();
            
            ctMode(getImageA(), -175, 275);
            
            //TODO:Re implement axes
            //if(thighAxes == null || stateChanged){
            //    thighAxes = new BuildThighAxes(getImageA(), 0);
            //    thighAxes.createAxes();
            //} //else they're loaded in loadVOI
            //added before button check so that they can be accessed in this way, optional to change.
            
            VOIVector vec = getImageA().getVOIs();
            for(int i=0; i<vec.size(); i++) {            
                for(int j=0; j<tabs.length; j++) {
                    if(tabs[j].hasButton(((VOI)vec.get(i)).getName())) {
                        tabs[j].setButton(((VOI)vec.get(i)).getName());
                    }
                }
            }
            
            
            
            updateImages(true);
            //componentRock = true;
        }
        
        
        
        
        
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
                //Find same voi, and remove it from original image
                if(((VOI)vector.get(i)).getName().equals(voiDialog.getObjectName())) {
                    removedVoi = (VOI)getImageA().getVOIs().remove(i);
                    break;
                }
            }
            VOIVector tempVOI = (VOIVector)getImageA().getVOIs().clone();
            VOIVector zeroVOI = getImageA().getVOIs();  //not cloned to maintain consistency of for loop
            
            //System.out.println("Size: "+getImageA().getVOIs().size());
            int k = zeroVOI.size();
            int j=0;
            int count = 0;
            while(count<k) {
                if(!(Boolean)zeroStatus.get(((VOI)zeroVOI.get(j)).getName())) {
                    ((ModelImage)getImageA()).getVOIs().remove(j);
                } else {
                    j++;
                }
                count++;
                //System.out.println("Size: "+getImageA().getVOIs().size());
            }
            
            srcImage.getVOIs().removeAllElements();
            if(removedVoi != null) {
                srcImage.registerVOI(removedVoi);
            }
            
            //Set intensities to zero, uncomment to add back in.
            
            BitSet fullMask = getImageA().generateVOIMask();

            //for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
            //    getImageA().set(i, REMOVED_INTENSITY);
            //}
            
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
                componentImage.getImageA().registerVOI(v);
               
            }
            componentImage.getImageA().clearMask();
            
            //getImageA().clearMask();
            //componentImage.setImageA(srcImage);
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
                Vector[][] contours = new Vector[2][];
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
                    
                
                    Color voiColor = thighVOIs[i].getColor();
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
                            yPoints[i + 2] = gons[elementNum].ypoints[i];
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

        private class MuscleDialogPrompt extends JPanel {
            
            //~ Static fields/initializers -------------------------------------------------------------------------------------
        
            public static final int REMOVED_INTENSITY = -2048;
            
            public static final String CHECK_BOX = "CHECK_BOX";
            
            public static final String DIALOG_COMPLETED = "DIALOG_COMPLETED";
            
            //~ Instance fields ------------------------------------------------------------------------------------------------
            
            /**
             * Vector list of objects that listen to this dialog box. When the action for this pseudo-algorithm has
             * completed, the program will notify all listeners.
             */
            private Vector objectList = new Vector();
            
            private JButton cancelButton;
            
            private JButton helpButton;
            
            private JButton OKButton;
            
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
            
            private MuscleImageDisplayTest parentFrame;
            
            //private ModelImage srcImg;
            
            /**
             * Creates new set of prompts for particular muscle.
             *
             * @param  theParentFrame  Parent frame.
             */
            public MuscleDialogPrompt(MuscleImageDisplayTest theParentFrame, String[] mirrorArr, boolean[] mirrorZ, 
                    String[] noMirrorArr, boolean[] noMirrorZ,  
                    ImageType imageType, Symmetry symmetry) {
                //super(theParentFrame, false);
                super();
                
                this.parentFrame = theParentFrame;
                
                this.mirrorArr = mirrorArr;
                this.noMirrorArr = noMirrorArr;
                
                this.mirrorZ = mirrorZ;
                this.noMirrorZ = noMirrorZ;
                
                this.imageType = imageType;
                this.symmetry = symmetry;
                
                
                init(mirrorArr, noMirrorArr);
                
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
                //System.out.println("Caught1: "+command);
                
                
            }
            
            

            public TreeMap getZeroStatus() {
                return zeroStatus;
            }
        
            
            private JPanel initInstructionPanel() {
                GridBagConstraints gbc = new GridBagConstraints();
                gbc.anchor = GridBagConstraints.WEST;
                gbc.fill = GridBagConstraints.HORIZONTAL;
                gbc.gridx = 0;
                gbc.gridy = 0;
                
                JPanel instructionPanel = new JPanel(new GridLayout(4, 1));
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
                
                VOIVector existingVois = ((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).getVOIs();
                zeroStatus = new TreeMap();
                 
                mirrorCheckArr = new JCheckBox[mirrorArr.length * 2];
                mirrorButtonArr = new JButton[mirrorArr.length * 2];
                ButtonGroup mirrorGroup = new ButtonGroup();
                JPanel mirrorPanel = new JPanel(new GridLayout(mirrorArr.length, 4));
                mirrorPanel.setForeground(Color.black);
                mirrorPanel.setBorder(MipavUtil.buildTitledBorder("Select an object"));
                
                GridBagConstraints gbc = new GridBagConstraints();
                gbc.anchor = GridBagConstraints.WEST;
                gbc.fill = GridBagConstraints.HORIZONTAL;
                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.ipadx = 0;
                
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
                    mirrorButtonArr[i].setActionCommand(MuscleImageDisplayTest.CHECK_VOI);
                    mirrorButtonArr[i].addActionListener(parentFrame);
                    mirrorGroup.add(mirrorButtonArr[i]);
                    
                    for(int j=0; j<existingVois.size(); j++) {
                        if(((VOI)existingVois.get(j)).getName().equals(mirrorButtonArr[i].getText())) {
                            mirrorCheckArr[i].setSelected(true);
                        }
                    }
                    
                    if(i != 0 && i % 4 == 0) {
                        gbc.gridy++;
                        gbc.gridx = 0;
                    }
                    //gbc.weightx = 0;
                    mirrorPanel.add(mirrorCheckArr[i], gbc);
                    gbc.gridx++;
                    //gbc.weightx = 1;
                    mirrorPanel.add(mirrorButtonArr[i], gbc);
                    gbc.gridx++;
                    
                    System.out.println(mirrorButtonArr[i].getText()+" is "+mirrorZ[i/2]);
                    zeroStatus.put(mirrorButtonArr[i].getText(), mirrorZ[i/2]);
                
                }          
                return mirrorPanel;
                        
            }
            
            private JPanel initNonSymmetricalObjects() {
                VOIVector existingVois = ((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).getVOIs();
                
                noMirrorCheckArr = new JCheckBox[noMirrorArr.length];
                noMirrorButtonArr = new JButton[noMirrorArr.length];
                ButtonGroup noMirrorGroup = new ButtonGroup();
                JPanel noMirrorPanel = new JPanel(new GridLayout(noMirrorButtonArr.length/2 + 1, 2));
                noMirrorPanel.setForeground(Color.black);
                noMirrorPanel.setBorder(MipavUtil.buildTitledBorder("Select an object"));
                GridBagConstraints gbc = new GridBagConstraints();
                gbc.anchor = GridBagConstraints.WEST;
                gbc.fill = GridBagConstraints.HORIZONTAL;
                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.ipadx = 0;
                for(int i=0; i<noMirrorArr.length; i++) {
                    noMirrorCheckArr[i] = new JCheckBox();
                    noMirrorCheckArr[i].setEnabled(false);
                    noMirrorCheckArr[i].setHorizontalAlignment(SwingConstants.RIGHT);
                    
                    noMirrorButtonArr[i] = new JButton(noMirrorArr[i]);
                    noMirrorButtonArr[i].setFont(MipavUtil.font12B);
                    noMirrorButtonArr[i].setActionCommand(MuscleImageDisplayTest.CHECK_VOI);
                    noMirrorButtonArr[i].addActionListener(parentFrame);
                    noMirrorGroup.add(noMirrorButtonArr[i]);
                    noMirrorPanel.add(noMirrorCheckArr[i]);
                    noMirrorPanel.add(noMirrorButtonArr[i]);
                    
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
            
            private void init(String[] mirrorArr, String[] noMirrorArr) {
                setForeground(Color.black);
                zeroStatus = new TreeMap();
                
                JPanel instructionPanel = initInstructionPanel();
                
                JPanel mirrorPanel = initSymmetricalObjects();
                
                JPanel noMirrorPanel = initNonSymmetricalObjects();
                
                JPanel mainPanel = new JPanel();
                mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
                
                mainPanel.add(instructionPanel);
                
                mainPanel.add(mirrorPanel);
        
                mainPanel.add(noMirrorPanel);
        
                mainPanel.add(buildButtons());
                
                add(mainPanel, BorderLayout.CENTER);
                
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
            public void notifyListeners() {
        
                for (int i = 0; i < objectList.size(); i++) {
                    ((ActionListener) objectList.elementAt(i)).actionPerformed(new ActionEvent(this, 0, DIALOG_COMPLETED));
                }
        
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
            
            /*public void notifyListeners(String command) {
                for (int i = 0; i < objectList.size(); i++) {
                    ((ActionListener) objectList.elementAt(i)).actionPerformed(new ActionEvent(this, 0, command));
                }
            }*/
        
            /**
             * Builds button panel consisting of OK, Cancel and Help buttons.
             *
             * @return  JPanel that has ok, cancel, and help buttons
             */
            protected JPanel buildButtons() {
                JPanel buttonPanel = new JPanel();
                
                cancelButton = new JButton("Cancel");
                cancelButton.addActionListener(parentFrame);
        
                // cancelButton.setToolTipText("Cancel action.");
                cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
                cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
                cancelButton.setFont(MipavUtil.font12B);
                
                helpButton = new JButton("Help");
                helpButton.addActionListener(parentFrame);
        
                // helpButton.setToolTipText("Find help for this screen.");
                helpButton.setMinimumSize(MipavUtil.defaultButtonSize);
                helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
                helpButton.setFont(MipavUtil.font12B);
                
                OKButton = new JButton("OK");
                OKButton.addActionListener(parentFrame);
        
                // OKButton.setToolTipText("Accept values and perform action.");
                OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
                OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
                OKButton.setFont(MipavUtil.font12B);
                
                buttonPanel.add(OKButton);
                buttonPanel.add(cancelButton);
                buttonPanel.add(helpButton);
        
                return buttonPanel;
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
    
    
}
