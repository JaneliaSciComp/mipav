import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewControlsImage;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewMenuBar;
import gov.nih.mipav.view.ViewMenuBuilder;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogWinLevel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.BitSet;
import java.util.TreeMap;
import java.util.Vector;
import javax.swing.*;
import javax.swing.event.ChangeListener;


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
    
    /** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Slice size = xDim*yDim */
    private int sliceSize;
    
    
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

        constructLog();

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
    
    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("MuscleSegmentation(" + ")\n");
    }
    
    private void performAbdomenDialog() {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        String[] mirrorArr = new String[3];
        mirrorArr[0] = "Psoas";
        mirrorArr[1] = "Lateral Abdominal";
        mirrorArr[2] = "Paraspinous";
        
        boolean[] mirrorZ = {true, true, true};
        
        String[] noMirrorArr = new String[2];
        noMirrorArr[0] = "Aortic calcium";
        noMirrorArr[1] = "Rectus abdominus";
        
        boolean[] noMirrorZ = {true, true};
        
        //MuscleImageDisplayTest display = new MuscleImageDisplayTest(((ViewJFrameImage)parentFrame).getImageA(), mirrorArr, mirrorZ, 
        //        noMirrorArr, noMirrorZ, ImageType.ABDOMEN, Symmetry.LEFT_RIGHT);

        
    }
    
    private void performThighDialog() {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        String[][] mirrorArr = new String[2][];
        mirrorArr[0] = new String[1];
        mirrorArr[0][0] = "Thigh";
        
        mirrorArr[1] = new String[5];
        mirrorArr[1][0] = "Quads";
        mirrorArr[1][1] = "Hamstrings";
        mirrorArr[1][2] = "Sartorius";
        mirrorArr[1][3] = "Fascia";
        mirrorArr[1][4] = "Abductors";
        
        boolean[][] mirrorZ = new boolean[2][];
        mirrorZ[0] = new boolean[1];
        mirrorZ[0][0] = false;
        
        mirrorZ[1] = new boolean[5];
        mirrorZ[1][0] = true;
        mirrorZ[1][1] = true;
        mirrorZ[1][2] = true;
        mirrorZ[1][3] = true;
        mirrorZ[1][4] = true;
        
        String[][] noMirrorArr = new String[2][];
        noMirrorArr[0] = new String[1];
        noMirrorArr[0][0] = "Phantom";
        
        noMirrorArr[1] = new String[1];
        noMirrorArr[1][0] = "Water sample";
        
        boolean[][] noMirrorZ = new boolean[2][];
        noMirrorZ[0] = new boolean[1];
        noMirrorZ[0][0] = false;
        
        noMirrorZ[1] = new boolean[1];
        noMirrorZ[1][0] = false;
        
        MuscleImageDisplayTest display = new MuscleImageDisplayTest(((ViewJFrameImage)parentFrame).getImageA(), mirrorArr, mirrorZ, 
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
        
        /** Whether this object has a mirror similar component in the image. */
        private Symmetry symmetry;
        
        /** handle to ViewUserInterface */
        private ViewUserInterface UI;
        
        
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
        
        private MuscleDialogPrompt muscle;
        
        private JButton OKButton;
        
        private JButton cancelButton;
        
        private MuscleImageDisplayTest parentFrame;
        
        
        public VoiDialogPrompt(MuscleDialogPrompt muscle, MuscleImageDisplayTest theParentFrame, String objectName, boolean closedVoi, int numVoi, Symmetry symmetry, TreeMap zeroStatus) {
            super();
            
            this.parentFrame = theParentFrame;
            
            this.objectName = objectName;
            this.closedVoi = closedVoi;
            this.numVoi = numVoi;
            this.symmetry = symmetry;
            this.muscle = muscle;
            
            //this.zeroStatus = zeroStatus;
            
            novelVoiProduced = false;
            completed = false;
            UI = ViewUserInterface.getReference();
            
            //initImage();
            initDialog();
            
        }
        
        public void actionPerformed(ActionEvent e) {
            String command = e.getActionCommand();
            if(command.equals(CLEAR)) {
                //clear all VOIs drawn
                ((ViewJFrameImage)parentFrame).getImageA().unregisterAllVOIs();
                ((ViewJFrameImage)parentFrame).updateImages();
            } else {

                if (command.equals("OK")) {
                    VOI goodVoi = checkVoi();
                    //check that VOI conforms to requirements, returns the VOI being modified/created
                    
                    if ( goodVoi != null ) { 
                        
                        //save modified/created VOI to file
                        ((ViewJFrameImage)parentFrame).getImageA().unregisterAllVOIs();
                        ((ViewJFrameImage)parentFrame).getImageA().registerVOI(goodVoi);
                        ((ViewJFrameImage)parentFrame).getImageA().getParentFrame().actionPerformed(new ActionEvent(this, 0, "Save all VOIs"));
                        
                        String fileDir = ((ViewJFrameImage)parentFrame).getImageA().getFileInfo(0).getFileDirectory();

                        MipavUtil.displayInfo(objectName+" VOI saved in folder\n " + fileDir + "defaultVOIs_DICOM");
                        
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
            String fileName = new String(((ViewJFrameImage)parentFrame).getImageA().getFileInfo(0).getFileDirectory()+"\\defaultVOIs_DICOM\\"+objectName+".xml");
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
            gbc.gridx++;
            mainPanel.add(buildButtons(), BorderLayout.SOUTH);
            
            add(mainPanel);
            //gbc.gridy++;

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
            
//          size and place the clear buttton
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

    
        
        private VOI checkVoi() {
            ModelImage voiImage = ((ModelImage)((ViewJFrameImage)parentFrame).getImageA());
            int qualifiedVoi = 0;
            VOIVector srcVOI = ((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).getVOIs();
            int countQualifiedVOIs = 0; //equal to 1 when the right  amount of VOIs have been created
            VOI goodVOI = null;
            //VOI goodVOI = (VOI)srcVOI.get(0);
            for(int i=0; i<srcVOI.size(); i++) {
                System.out.println(((VOI)srcVOI.get(i)).getName());
                if(((VOI)srcVOI.get(i)).getName().equals(objectName)) {
                    goodVOI = ((VOI)srcVOI.get(i));
                    countQualifiedVOIs++;
                    qualifiedVoi = 1;
                }
            }
            
            
            String[] mirrorArr = muscle.getMirrorButtonArr();
            String[] noMirrorArr = muscle.getNoMirrorButtonArr();
            for(int i=0; i<srcVOI.size(); i++) {
                //if(qualifiedVoi != 1) {
                    int count = 0;
                    System.out.println(((VOI)srcVOI.get(i)).getName());
                    
                    
                    for(int j=0; j<mirrorArr.length; j++) {
                        if(((VOI)srcVOI.get(i)).getName().equals(mirrorArr[j])) {
                            count++;
                        }
                    }
                    for(int j=0; j<noMirrorArr.length; j++) {
                        if(((VOI)srcVOI.get(i)).getName().equals(noMirrorArr[j])) {
                            count++;
                        }
                    }
                    if(count == 0) {
                        qualifiedVoi++;
                        goodVOI = (VOI)srcVOI.get(i);
                        
                    } else {
                        //qualifiedVoi = 5;
                    }
                //}
                
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
    
    public class MuscleImageDisplayTest extends ViewJFrameImage implements ActionListener {
        
        public static final String CHECK_VOI = "CHECK_VOI";
        
        /**
         * Check boxes for mirror muscle buttons. 
         */
        private JCheckBox[] mirrorCheckArr;

        /**
         * Check boxes for non-mirror object buttons. 
         */
        private JCheckBox[] noMirrorCheckArr;

        /**
         * Buttons for muscles where a mirror muscle may exist. 
         */
        private JButton[] mirrorButtonArr;

        /**
         * Buttons for muscles where mirror muscles are not considered. 
         */
        private JButton[] noMirrorButtonArr;

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
        
        private TreeMap identifiers;
        
        private MuscleDialogPrompt[] tabs;
        
        public MuscleImageDisplayTest(ModelImage image, 
                String[][] mirrorArr, boolean[][] mirrorZ, 
                String[][] noMirrorArr, boolean[][] noMirrorZ,  
                ImageType imageType, Symmetry symmetry) {
            
            
            super(image);
            //super(image, true);
            //if we don't have an image, then we're done
            
            this.setImageA(image);
            
            this.mirrorArr = mirrorArr;
            this.mirrorZ = mirrorZ;
            this.noMirrorArr = noMirrorArr;
            this.noMirrorZ = noMirrorZ;
            this.imageType = imageType;
            this.symmetry = symmetry;
            
            this.identifiers = new TreeMap();
            
            if (imageA == null) {
                return;
            }
            
            
            initNext();

        }
        
        private JPanel initDialog() {
//          The component image will be displayed in a scrollpane.
            scrollPane = new JScrollPane(componentImage, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                         JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

            imagePane = new JTabbedPane();
            
            tabs = new MuscleDialogPrompt[mirrorArr.length];

            for(int i=0; i<tabs.length; i++) {
                tabs[i] = new MuscleDialogPrompt(this, mirrorArr[i], mirrorZ[i],
                                                    noMirrorArr[i], noMirrorZ[i],
                                                    imageType, symmetry);
                
                tabs[i].addListener(this);
                imagePane.addTab(i+": Object", tabs[i]);
                                                                            
            }
            
            JPanel panelA = new JPanel(new GridLayout(1, 3, 10, 10));
            
            splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, imagePane, scrollPane);
            scrollPane.setBackground(Color.black);
            panelA.add(splitPane);
            
            return panelA;
            
        }
        
        private void initNext() {
            JPanel panelA = initDialog();
            getContentPane().add(panelA);
            getContentPane().remove(0);
            Container c = getContentPane();
            
            pack();
            initMuscleImage();
        }
        
        /**
         * Resizes frame and all components.
         *
         * @param  event  event that triggered function
         */
        public synchronized void componentResized(ComponentEvent event) {
            
            System.out.println("Resizing here.");
            
        }
        
        public void lockToVOI(VoiDialogPrompt voi) {
            System.out.println("Got here.");
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
            initMuscleImage();
        }
        
        private void initMuscleImage() {
            componentImage.getImageA().unregisterAllVOIs();
            int colorChoice = 0;
            String fileDir = componentImage.getImageA().getFileInfo(0).getFileDirectory()+"defaultVOIs_DICOM\\";
            File allVOIs = new File(fileDir);
            if(allVOIs.isDirectory()) {
                String[] voiName = allVOIs.list();
                for(int i=0; i<voiName.length; i++) {
                    if(new File(fileDir+voiName[i]).exists()) {
                        String fileName = voiName[i];
                        FileVOI v;
                        VOI[] voiVec = null;
                        try {
                            v = new FileVOI(fileName, fileDir, componentImage.getImageA());
                            voiVec = v.readVOI(false);
                        } catch(IOException e) {
                            MipavUtil.displayError("Unable to load old VOI from location:\n"+fileDir+"\nWith name: "+fileName);
                        }
                        if(voiVec.length > 1) {
                            MipavUtil.displayError("Invalid VOI from location:\n"+fileDir+"\nWith name: "+fileName);
                        } else {
                            Color c = hasColor(voiVec);
                            if(c != null) {
                                voiVec[0].setColor(c);
                                
                            } else {
                                voiVec[0].setColor(colorPick[colorChoice % colorPick.length]);
                                colorChoice++;
                            }                      
                            componentImage.getImageA().registerVOI(voiVec[0]);
                            for(int j=0; j<tabs.length; j++) {
                                if(tabs[j].hasButton(voiVec[0].getName())) {
                                    tabs[j].setButton(voiVec[0].getName());
                                }
                            }
                            
                        }
                    }
                }
            }
            updateImages(true);
        }
        
        private Color hasColor(VOI[] voiVec) {
            Color c = null;
            VOIVector tempVec = imageA.getVOIs();
            for(int i=0; i<tempVec.size(); i++) {
                if(voiVec[0].getName().contains("Left") || voiVec[0].getName().contains("Right")) {
                    if( !(((VOI)tempVec.get(i)).getName().contains("Left")  &&  voiVec[0].getName().contains("Left")) && 
                            !(((VOI)tempVec.get(i)).getName().contains("Right")  &&  voiVec[0].getName().contains("Right")) && 
                            ((VOI)tempVec.get(i)).getName().endsWith(voiVec[0].getName().substring(voiVec[0].getName().indexOf(" ")))) {
                        c =  ((VOI)tempVec.get(i)).getColor();
                    }
                }
            }
            return c;
        }
        
    }
    
    private class MuscleDialogPrompt extends JPanel implements ActionListener{
        
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
        
        /** handle to ViewUserInterface */
        private ViewUserInterface UI;
        
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
            
            UI = ViewUserInterface.getReference();
            
            init(mirrorArr, noMirrorArr);
            
        }
        
        
        private void initVoiImage(VoiDialogPrompt voiDialog) {
            srcImage = (ModelImage)((ViewJFrameImage)parentFrame).getImageA().clone();
            
            VOIVector vector = srcImage.getVOIs();
            VOI removedVoi = null;
            for(int i=0; i < vector.size() ; i++) {
                //Find same voi, and remove it from original image
                if(((VOI)vector.get(i)).getName().equals(voiDialog.getObjectName())) {
                    removedVoi = (VOI)((ViewJFrameImage)parentFrame).getImageA().getVOIs().remove(i);
                    break;
                }
            }
            VOIVector tempVOI = (VOIVector)((ViewJFrameImage)parentFrame).getImageA().getVOIs().clone();
            VOIVector zeroVOI = ((ViewJFrameImage)parentFrame).getImageA().getVOIs();  //not cloned to maintain consistency of for loop
            
            System.out.println("Size: "+((ViewJFrameImage)parentFrame).getImageA().getVOIs().size());
            
            for(int i=0; i<zeroVOI.size(); i++) {
                if(!(Boolean)zeroStatus.get(((VOI)zeroVOI.get(i)).getName())) {
                    ((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).getVOIs().remove(i);
                }
                System.out.println("Size: "+((ViewJFrameImage)parentFrame).getImageA().getVOIs().size());
            }
            
            srcImage.getVOIs().removeAllElements();
            if(removedVoi != null) {
                srcImage.registerVOI(removedVoi);
            }
            
            //Set intensities to zero, uncomment to add back in.
            
            BitSet fullMask = ((ViewJFrameImage)parentFrame).getImageA().generateVOIMask();

            for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
                srcImage.set(i, REMOVED_INTENSITY);
            }
            
            srcImage.setMask(fullMask);
            
            //Display VOI of current objects.
            
            for(int i=0; i<tempVOI.size(); i++) {
                VOI v = (VOI)tempVOI.get(i);
                if((Boolean)zeroStatus.get(v.getName())) {
                    v.setDisplayMode(VOI.SOLID);
                } else {
                    v.setDisplayMode(VOI.CONTOUR);
                }
                srcImage.registerVOI(v);
               
            }
            
            ((ViewJFrameImage)parentFrame).getImageA().clearMask();
            ((ViewJFrameImage)parentFrame).setImageA(srcImage);
            ((ViewJFrameImage)parentFrame).updateImages();
            //LOOK HERE
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
            if(command.equals(MuscleImageDisplayTest.CHECK_VOI)) {
                //initVoiImage();
                String voiString = ((JButton)(e.getSource())).getText();
                VoiDialogPrompt voiPrompt = new VoiDialogPrompt(this, parentFrame, voiString, true, 1, Symmetry.LEFT_RIGHT, zeroStatus);
                // This is very important. Adding this object as a listener allows the subdialog to
                // notify this object when it has completed or failed. 
                // This is could be generalized by making a subDialog interface.
                voiPrompt.addListener(this);
                initVoiImage(voiPrompt);
                ((MuscleImageDisplayTest)parentFrame).lockToVOI(voiPrompt);
                //setVisible(false); // Hide dialog
            } else if(command.equals(VoiDialogPrompt.SUB_DIALOG_COMPLETED)) {
                ((MuscleImageDisplayTest)parentFrame).unlockToVOI();
            }
            
            /*String command = e.getActionCommand();
            System.out.println("This button "+e.getActionCommand());
            if (command.equals("OK")) {
                
                //make sure all buttons are green
                if (checkVariables()) { 
                    //now what?
                }
            } else if (command.equals("Cancel")) {
                //should dispose MuscleImageDisplay
                //dispose();
            } else if (command.equals("Help")) {
                System.out.println("Over here");
                MipavUtil.showHelp("19014");
            }
            
            notifyListeners();*/
            
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
            mirrorPanel.setBorder(MipavUtil.buildTitledBorder("Select a muscle"));
            
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
                mirrorButtonArr[i].addActionListener(this);
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
                noMirrorButtonArr[i].addActionListener(this);
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
            
            JPanel mainPanel = new JPanel(new GridLayout(4, 1));
            
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.gridx = 0;
            gbc.gridy = 0;
            
            mainPanel.add(instructionPanel, gbc);
            gbc.gridy++;
            
            mainPanel.add(mirrorPanel, gbc);
            gbc.gridy++;
            
            mainPanel.add(noMirrorPanel, gbc);
            gbc.gridy++;
            
            mainPanel.add(buildButtons(), gbc);
            
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
            cancelButton.addActionListener(this);

            // cancelButton.setToolTipText("Cancel action.");
            cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
            cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
            cancelButton.setFont(MipavUtil.font12B);
            
            helpButton = new JButton("Help");
            helpButton.addActionListener(this);

            // helpButton.setToolTipText("Find help for this screen.");
            helpButton.setMinimumSize(MipavUtil.defaultButtonSize);
            helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
            helpButton.setFont(MipavUtil.font12B);
            
            OKButton = new JButton("OK");
            OKButton.addActionListener(this);

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
}
