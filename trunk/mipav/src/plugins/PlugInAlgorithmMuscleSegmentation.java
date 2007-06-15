import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.util.BitSet;
import java.util.Vector;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;


/**
 * Does all the stuff just about.
 * 
 * @author senseneyj
 *
 */


public class PlugInAlgorithmMuscleSegmentation extends AlgorithmBase {
    
    //~ Static fields --------------------------------------------------------------------------------------------------
    
    public static final Color[] colorPick = {Color.BLUE, Color.RED, Color.GREEN, Color.ORANGE, Color.CYAN, Color.PINK, Color.YELLOW, Color.MAGENTA, Color.WHITE};
    
    //~ Instance fields ------------------------------------------------------------------------------------------------    
    
    /** denotes the type of srcImg (see enum ImageType) */
    private ImageType imageType;
    
    /** the parent frame. */
    private Frame parentFrame;
    
    /** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Slice size for xDim*yDim */
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
        
        
        
        
        
    }
    
    private void performThighDialog() {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        String[] mirrorArr = new String[4];
        mirrorArr[0] = "Quads";
        mirrorArr[1] = "Hamstrings";
        mirrorArr[2] = "Sartorius";
        mirrorArr[3] = "Everything else";
        
        String[] noMirrorArr = new String[2];
        noMirrorArr[0] = "Phantom";
        noMirrorArr[1] = "Block thing";
        
        MuscleDialogPrompt prompt = new MuscleDialogPrompt(parentFrame, mirrorArr, noMirrorArr, ImageType.TWO_THIGHS, Symmetry.LEFT_RIGHT);
        
        
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

    private class VoiDialogPrompt extends JDialogBase {
        
        //~ Static fields/initializers -------------------------------------------------------------------------------------

        public static final String CLEAR = "Clear";
        
        public static final int REMOVED_INTENSITY = -2048;
        
        //~ Instance fields ------------------------------------------------------------------------------------------------
        
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
        
        
        public VoiDialogPrompt(Frame theParentFrame, String objectName, boolean closedVoi, int numVoi, Symmetry symmetry) {
            super(theParentFrame, false);
            
            
            
            this.objectName = objectName;
            this.closedVoi = closedVoi;
            this.numVoi = numVoi;
            this.symmetry = symmetry;
            
            novelVoiProduced = false;
            completed = false;
            UI = ViewUserInterface.getReference();
            
            initImage();
            init();
            
        }
        
        public void actionPerformed(ActionEvent e) {
            String command = e.getActionCommand();
            if(command.equals(CLEAR)) {
                //clear all VOIs drawn
                srcImage.unregisterAllVOIs();
                srcImage.getParentFrame().updateImages();
            } else {

                if (command.equals("OK")) {
                    
                    //check that VOI conforms to requirements
                    if (checkVoi()) { 
                        
                        //save VOI to file
                        ((VOI)(srcImage.getVOIs().get(0))).setName(objectName);
                        srcImage.getParentFrame().actionPerformed(new ActionEvent(this, 0, "Save all VOIs"));
                        
                        String fileDir = ((ViewJFrameImage)parentFrame).getImageA().getFileInfo(0).getFileDirectory();

                        MipavUtil.displayInfo(objectName+" VOI saved in folder\n " + fileDir + "defaultVOIs_DICOM");
                        
                        completed = true;
                        novelVoiProduced = true;
                        notifyListeners(this);
                        dispose();
                    } else {
                        //individual error messages are already displayed
                    }
                } else if (command.equals("Cancel")) {
                    setCompleted(savedVoiExists());
                    notifyListeners(this);
                    dispose();
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
        public void addListener(MuscleDialogPrompt obj) {
            objectList.addElement(obj);
        }
        
        /**
         * Used to notify all listeners that the pseudo-algorithm has completed.
         *
         * @param  dialog the sub-dialog that has completed the function
         */
        public void notifyListeners(VoiDialogPrompt dialog) {

            for (int i = 0; i < objectList.size(); i++) {
                ((MuscleDialogPrompt) objectList.elementAt(i)).subDialogCompleted(dialog);
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
        
        private void init() {
            setForeground(Color.black);
            addNotify();
            setTitle("VOI selection");
                        
            getContentPane().setLayout(new BorderLayout());
            
            JPanel mainPanel = new JPanel(new GridLayout(1, 1));
            
            mainPanel.setForeground(Color.black);
            mainPanel.setBorder(MipavUtil.buildTitledBorder("VOI Selection"));
            
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.WEST;
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
            mainPanel.add(label, gbc);
            
            getContentPane().add(mainPanel, BorderLayout.NORTH);
            
            //Build the Panel that holds the OK, Clear, and Cancel Buttons
            JPanel OKClearCancelPanel = new JPanel();

            // size and place the ok button
            buildOKButton();
            OKClearCancelPanel.add(OKButton, BorderLayout.WEST);
            
            // size and place the clear buttton
            clearButton = new JButton("Clear");
            clearButton.addActionListener(this);
            clearButton.setActionCommand(CLEAR);
            clearButton.setToolTipText("Clear the VOI.");
            clearButton.setMinimumSize(MipavUtil.defaultButtonSize);
            clearButton.setPreferredSize(MipavUtil.defaultButtonSize);
            clearButton.setFont(serif12B);
            OKClearCancelPanel.add(clearButton, BorderLayout.CENTER);
            
            // size and place the cancel button
            buildCancelButton();
            OKClearCancelPanel.add(cancelButton, BorderLayout.EAST);
            getContentPane().add(OKClearCancelPanel, BorderLayout.SOUTH);

            pack();
            setResizable(false);
            setVisible(true);
        }
        
        /**
         * Initializes the image.
         *
         */
        
        private void initImage() {
            srcImage = (ModelImage)((ViewJFrameImage)parentFrame).getImageA().clone();
            
            VOIVector vector = srcImage.getVOIs();
            VOI removedVoi = null;
            for(int i=0; i < vector.size() ; i++) {
                //Find same voi, and remove it from original image
                if(((VOI)vector.get(i)).getName().equals(objectName)) {
                    removedVoi = (VOI)((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).getVOIs().remove(i);
                    break;
                }
            }
            VOIVector tempVOI = ((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).getVOIs();
            srcImage.getVOIs().removeAllElements();
            if(removedVoi != null) {
                srcImage.registerVOI(removedVoi);
            }
            BitSet fullMask = ((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).generateVOIMask();

            for(int i=fullMask.nextSetBit(0); i>=0; i=fullMask.nextSetBit(i+1)) {
                srcImage.set(i, REMOVED_INTENSITY);
            }
            
            srcImage.setMask(fullMask);
            
            for(int i=0; i<tempVOI.size(); i++) {
                VOI v = (VOI)tempVOI.get(i);
                BitSet tempSet = new BitSet();
                v.createBinaryMask(tempSet, xDim, yDim);
               
            }
            
            ((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).clearMask();
            ViewJFrameImage newFrame = new ViewJFrameImage(srcImage);
            newFrame.updateImages();
        }
        
        private boolean checkVoi() {
            //ModelImage voiImage = ((ViewJFrameImage)parentFrame).getImageA();
            if(srcImage.getVOIs().size() != 1) {
                String error = srcImage.getVOIs().size() > 1 ? "You have created too many VOIs." : 
                                                                "You haven't created any VOIs.";
                MipavUtil.displayError(error);
                return false;  
            }
            Vector[] curves = ((VOI)(srcImage.getVOIs().get(0))).getCurves();
            VOI voi = ((VOI)(srcImage.getVOIs().get(0)));
            if(((Vector)curves[0]).size() == numVoi) {
                for(int i=0; i<numVoi; i++) {
                    if(closedVoi && voi.getCurveType() == VOI.CONTOUR) {
                        return true;
                    } else if(!closedVoi && voi.getCurveType() != VOI.CONTOUR) {
                        return true;
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
            return false;
        }
        
    }
    
    private class MuscleDialogPrompt extends JDialogBase {
        
        //~ Static fields/initializers -------------------------------------------------------------------------------------

        public static final String CHECK_VOI = "CHECK_VOI";
        
        public static final String CHECK_BOX = "CHECK_BOX";
        
        //~ Instance fields ------------------------------------------------------------------------------------------------
        
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
        
        
        //private ModelImage srcImg;
        
        /**
         * Creates new set of prompts for particular muscle.
         *
         * @param  theParentFrame  Parent frame.
         */
        public MuscleDialogPrompt(Frame theParentFrame, String[] mirrorArr, String[] noMirrorArr, 
                ImageType imageType, Symmetry symmetry) {
            super(theParentFrame, false);
            this.imageType = imageType;
            this.symmetry = symmetry;
            
            UI = ViewUserInterface.getReference();
            
            
            initImage();
            init(mirrorArr, noMirrorArr);
            
        }
        
        public void actionPerformed(ActionEvent e) {
            
            String command = e.getActionCommand();
            if(command.equals(CHECK_VOI)) {
                
                ((ViewJFrameImage)parentFrame).setVisible(false);
                
                VoiDialogPrompt voiPrompt = new VoiDialogPrompt(parentFrame, ((JButton)(e.getSource())).getText(), true, 1, symmetry);
                // This is very important. Adding this object as a listener allows the subdialog to
                // notify this object when it has completed or failed. 
                // This is could be generalized by making a subDialog interface.
                voiPrompt.addListener(this);

                setVisible(false); // Hide dialog
            } else {

                if (command.equals("OK")) {
                    
                    //make sure all buttons are green
                    if (checkVariables()) { 
                        //now what?
                    }
                } else if (command.equals("Cancel")) {
                    dispose();
                } else if (command.equals("Help")) {
                    MipavUtil.showHelp("19014");
                }
            }
            
        }
        
        //subDialog completed, not neccesarily successfully
        public void subDialogCompleted(VoiDialogPrompt dialog) {
            findButton(dialog, dialog.isCompleted());
            initImage();
            srcImage.getParentFrame().setVisible(false);
            ((ViewJFrameImage)parentFrame).setVisible(true);
            setVisible(true);
        }
        
        private boolean findButton(VoiDialogPrompt dialog, boolean completed) {
            int index = -1;
            for(int i=0; i<mirrorButtonArr.length; i++) {
                if(mirrorButtonArr[i].getText().equals(dialog.getObjectName())) {
                    index = i;
                }
            }
            if(index == -1) {
                for(int i=0; i<noMirrorButtonArr.length; i++) {
                    if(noMirrorButtonArr[i].getText().equals(dialog.getObjectName())) {
                        index = i;
                    }
                }
                noMirrorCheckArr[index].setSelected(completed);
            } else {
                mirrorCheckArr[index].setSelected(completed);
            }
            if(index == -1) {
                return false;
            } else {
                return true;
            }
        }
        
        /**
         * Initializes the image.
         *
         */
        
        private void initImage() {
            ((ViewJFrameImage)parentFrame).getImageA().unregisterAllVOIs();
            int colorChoice = 0;
            String fileDir = ((ViewJFrameImage)parentFrame).getImageA().getFileInfo(0).getFileDirectory()+"defaultVOIs_DICOM\\";
            File allVOIs = new File(fileDir);
            if(allVOIs.isDirectory()) {
                String[] voiName = allVOIs.list();
                for(int i=0; i<voiName.length; i++) {
                    if(new File(fileDir+voiName[i]).exists()) {
                        String fileName = voiName[i];
                        FileVOI v;
                        VOI[] voiVec = null;
                        try {
                            v = new FileVOI(fileName, fileDir, ((ViewJFrameImage)parentFrame).getImageA());
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
                            ((ViewJFrameImage)parentFrame).getImageA().registerVOI(voiVec[0]);
                        }
                    }
                }
            }
            ((ViewJFrameImage)parentFrame).updateImages();
                
            
        }
        
        private Color hasColor(VOI[] voiVec) {
            Color c = null;
            int j = 2+ 2;
            VOIVector tempVec = ((ViewJFrameImage)parentFrame).getImageA().getVOIs();
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
        
        private void init(String[] mirrorArr, String[] noMirrorArr) {
            setForeground(Color.black);
            addNotify();
            String title;
            if(imageType.equals(ImageType.ABDOMEN)) {
                title = "Abdomen segmentation";
            } else if(imageType.equals(ImageType.TWO_THIGHS)) {
                title = "Thigh segmentation";
            } else {
                title = "Muscle segmentation";
            }
            setTitle(title);
            
            getContentPane().setLayout(new BorderLayout());
            
            JPanel instructionPanel, mirrorPanel, noMirrorPanel, mainPanel;
            
            mainPanel = new JPanel(new BorderLayout());
            
            instructionPanel = new JPanel(new GridLayout(4, 1));
            instructionPanel.setForeground(Color.black);
            instructionPanel.setBorder(MipavUtil.buildTitledBorder("Instructions"));
            instructionLabel = new JLabel[4];
            instructionLabel[0] = new JLabel("1) Press an object button.\n\r");
            instructionLabel[1] = new JLabel("2) A dialog box will prompt you to draw VOI(s) around that object.");
            instructionLabel[2] = new JLabel("3) Once drawn the check box next to the button will be checked.");
            instructionLabel[3] = new JLabel("4) Press that button again to review your VOI(s).");
            
            VOIVector existingVois = ((ModelImage)((ViewJFrameImage)parentFrame).getImageA()).getVOIs();
            
            
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.ipadx = 0;
            for(int i=0; i<instructionLabel.length; i++) {
                instructionLabel[i].setFont(MipavUtil.font12);
                instructionPanel.add(instructionLabel[i], gbc);
                gbc.gridy++;
            }
            
            mainPanel.add(instructionPanel, BorderLayout.NORTH);
            
            if(!symmetry.equals(Symmetry.NO_SYMMETRY)) {
                
                mirrorCheckArr = new JCheckBox[mirrorArr.length * 2];
                mirrorButtonArr = new JButton[mirrorArr.length * 2];
                ButtonGroup mirrorGroup = new ButtonGroup();
                mirrorPanel = new JPanel(new GridLayout(mirrorArr.length, 4));
                mirrorPanel.setForeground(Color.black);
                mirrorPanel.setBorder(MipavUtil.buildTitledBorder("Select a muscle"));
                
                //GridBagConstraints gbc = new GridBagConstraints();
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
                    //mirrorButtonArr[i].setPreferredSize(MipavUtil.defaultButtonSize);
                    mirrorButtonArr[i].setFont(serif12B);
                    mirrorButtonArr[i].setActionCommand(CHECK_VOI);
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
                    gbc.weightx = 0;
                    mirrorPanel.add(mirrorCheckArr[i]);
                    gbc.gridx++;
                    gbc.weightx = 1;
                    mirrorPanel.add(mirrorButtonArr[i]);
                    gbc.gridx++;
                    
                }
                
                mainPanel.add(mirrorPanel, BorderLayout.CENTER);
            }
            noMirrorCheckArr = new JCheckBox[noMirrorArr.length];
            noMirrorButtonArr = new JButton[noMirrorArr.length];
            ButtonGroup noMirrorGroup = new ButtonGroup();
            noMirrorPanel = new JPanel(new GridLayout(noMirrorButtonArr.length/2 + 1, 2));
            noMirrorPanel.setForeground(Color.black);
            noMirrorPanel.setBorder(MipavUtil.buildTitledBorder("Select an object"));
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
                //noMirrorButtonArr[i].setPreferredSize(MipavUtil.defaultButtonSize);
                noMirrorButtonArr[i].setFont(serif12B);
                noMirrorButtonArr[i].setActionCommand(CHECK_VOI);
                noMirrorButtonArr[i].addActionListener(this);
                noMirrorGroup.add(noMirrorButtonArr[i]);
                noMirrorPanel.add(noMirrorCheckArr[i]);
                noMirrorPanel.add(noMirrorButtonArr[i]);
                
                for(int j=0; j<existingVois.size(); j++) {
                    if(((VOI)existingVois.get(j)).getName().equals(noMirrorButtonArr[i].getText())) {
                        noMirrorCheckArr[i].setSelected(true);
                    }
                }
            }
            
            mainPanel.add(noMirrorPanel, BorderLayout.SOUTH);

            getContentPane().add(mainPanel, BorderLayout.NORTH);
            getContentPane().add(buildButtons(), BorderLayout.SOUTH);

            pack();
            setResizable(false);
            setVisible(true);
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


        
    }
}
