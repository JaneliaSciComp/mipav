package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Creates the dialog to insert missing slices into an image. The dialog use the
 * origin[2] for each slice to see if there are any missing slices.  If so, either
 * an average or a blank can be inserted.
 */
public class JDialogInsertMissingSlices extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** If true insert blank slices, if false insert weighted average slices */
    private boolean insertBlank;

    /** Radio button selected if inserted slices are a weighted average of
     *  surrounding slices */
    private JRadioButton average;

    /** Radio button selected if inserted slices are blank */
    private JRadioButton blank;

    
    /** source image */
    private ModelImage image;

    /** true if no slices are missing */
    private boolean allPresent;

    /** DOCUMENT ME! */
    private AlgorithmReplaceRemovedSlices rSliceAlgo;

    /** Number of slices in original 3D image */
    private int nSlices;

    /** image create if new image button is selected */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private ButtonGroup destinationGroup;
    
    private JRadioButton newImage;
    
    private JRadioButton replaceImage;
    
    /** Flag indicating if a new image is to be generated */
    private int displayLoc;
    
    /** Array of length totalSlices, false where slice is already present,
        true where slice must be inserted */
    private boolean[] checkListInsert;
    
    /** Number of slices that will be present in the 3D image after the
     *  missing slices have been inserted */
    private int totalSlices;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogInsertMissingSlices() { }

    /**
     * Creates new dialog for inserting a slice.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogInsertMissingSlices(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im; // set the image from the arguments to an image in this class
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogInsertMissingSlices(ViewUserInterface UI, ModelImage im) {
        super(false);
        userInterface = UI;
        image = im;
        parentFrame = im.getParentFrame();
        setUpCheckList();
        
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    private void setUpCheckList() {
        int m;
        float averageSpacing;
        int z;
        float spacing;
        int numSlices;
        int i;
        
        averageSpacing = (image.getFileInfo(nSlices-1).getOrigin()[2] -
                image.getFileInfo(0).getOrigin()[2])/
                (nSlices - 1);
        // Count first slice
        totalSlices = 1;
        for (z = 0; z < nSlices - 1; z++) {
          spacing = image.getFileInfo(z+1).getOrigin()[2] 
                    - image.getFileInfo(z).getOrigin()[2];
          numSlices = Math.max(1,Math.round(spacing/averageSpacing));
          totalSlices += numSlices;
        }
        
        allPresent = true;
        checkListInsert = new boolean[totalSlices];
        checkListInsert[0] = false;
        for (z = 0, m = 1; z < nSlices - 1; z++) {
          spacing = image.getFileInfo(z+1).getOrigin()[2] 
                    - image.getFileInfo(z).getOrigin()[2];
          numSlices = Math.max(1,Math.round(spacing/averageSpacing));
          for (i = 0; i < numSlices - 1; i++) {
              checkListInsert[m++] = true;
              allPresent = false;
          }
          checkListInsert[m++] = false;
        }   
    }
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("");
        } 
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmReplaceRemovedSlices) {
            
            if (displayLoc == NEW) {

                if ((rSliceAlgo.isCompleted() == true) && (resultImage != null)) {

                    try {

                        // put the new image into a new frame
                        new ViewJFrameImage(resultImage, null, new Dimension(25, 32));

                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Insert missing Slices reports: out of memory; " + "unable to open a new frame");
                        return;
                    }

                }
            } else {

                if (rSliceAlgo.isCompleted() == true) {
                    image.notifyImageExtentsListeners();
                }
            }
        }

        if (algorithm.isCompleted() && (algorithm instanceof AlgorithmReplaceRemovedSlices)) {
            insertScriptLine(algorithm);
        }

        algorithm.finalize();
        algorithm = null;
        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("InsertMissingSlices " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");

                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                           " ");
                } else {
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                           " ");
                }

                userInterface.getScriptDialog().append(insertBlank + "\n");
            }
        }
    }
    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        setModal(false);
        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }
        
        if (allPresent) {
            destImageKey = srcImageKey;
        }
        
        if (srcImageKey.equals(destImageKey)) {
            this.setDisplayLocReplace();
        } else {
            this.setDisplayLocNew();
        }

        try {
            insertBlank = parser.getNextBoolean();
            setInsertBlank(insertBlank);

            
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
        callAlgorithm();
        if (!srcImageKey.equals(destImageKey)) {
            parser.putVariable(destImageKey, getResultImage().getImageName());
        }
    }

    
    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }
    

    /**
     * Accessor which lets you change the type of slice to be inserted.
     *
     * @param  inseertBlank  the type of slice to be inserted (either AVERAGE_SLICE or BLANK_Slice)
     */
    public void setInsertBlank(boolean insertBlank) {
        this.insertBlank = insertBlank;
    }

    

    /**
     * Once all the necessary variables are set, call the Insert Slice algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    private void callAlgorithm() {
        
        if (allPresent) {
            Preferences.debug(image.getImageName() + " has no missing slices\n");
            dispose();
            return;
        }

        

        try {

            
            if (displayLoc == NEW) {
                resultImage = (ModelImage)image.clone();
                resultImage.setImageName(image.getImageName() + "_insertedMisssingSlices");
            }

            // Make algorithm:
            if (displayLoc == REPLACE) {
                rSliceAlgo = new AlgorithmReplaceRemovedSlices(image, checkListInsert, false, false,
                                                                      insertBlank);
            } else {
                rSliceAlgo = new AlgorithmReplaceRemovedSlices(resultImage, checkListInsert, false, false,
                                                                      insertBlank);
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            rSliceAlgo.addListener(this);
            setVisible(false); // Hide dialog

            if (runInSeparateThread) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (rSliceAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                rSliceAlgo.setActiveImage(isActiveImage);

                if (!userInterface.isAppFrameVisible()) {
                    rSliceAlgo.setProgressBarVisible(false);
                }

                rSliceAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up image memory
                resultImage = null;
            }

            MipavUtil.displayError("Insert Missing Slices reports: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        float averageSpacing;
        float spacing;
        int z, i, m, k;
        int numSlices;
        int yPos = 0;
        JLabel statusLabel = null;
        int missingSlices;
        int missingArray[];
        nSlices = image.getExtents()[2];

        setTitle("Insert missing slices");
        setForeground(Color.black);

        JPanel slicePanel = new JPanel(new GridBagLayout());
        slicePanel.setBorder(buildTitledBorder("Insert missing slices"));
        
        averageSpacing = (image.getFileInfo(nSlices-1).getOrigin()[2] -
                image.getFileInfo(0).getOrigin()[2])/
                (nSlices - 1);
        // Count first slice
        totalSlices = 1;
        for (z = 0; z < nSlices - 1; z++) {
          spacing = image.getFileInfo(z+1).getOrigin()[2] 
                    - image.getFileInfo(z).getOrigin()[2];
          numSlices = Math.max(1,Math.round(spacing/averageSpacing));
          totalSlices += numSlices;
        }
        missingSlices = totalSlices - nSlices;
        missingArray = new int[missingSlices];
        
        allPresent = true;
        checkListInsert = new boolean[totalSlices];
        checkListInsert[0] = false;
        for (z = 0, m = 1, k = 0; z < nSlices - 1; z++) {
          spacing = image.getFileInfo(z+1).getOrigin()[2] 
                    - image.getFileInfo(z).getOrigin()[2];
          numSlices = Math.max(1,Math.round(spacing/averageSpacing));
          if (numSlices == 2) {
              Preferences.debug("1 slice is missing between " + (z+1) +
                  " and " + (z+2) + "\n");
          }
          else if (numSlices > 2) {
              Preferences.debug((numSlices-1) + " are missing between " + (z+1) +
                  " and " + (z+2) + "\n");
          }
          for (i = 0; i < numSlices - 1; i++) {
              checkListInsert[m++] = true;
              allPresent = false;
              missingArray[k++] = z;
          }
          checkListInsert[m++] = false;
        }
        
        if (allPresent) {
            statusLabel = new JLabel("No slices are missing");
        }
        else if (missingSlices == 1){
            statusLabel = new JLabel("Slice between " + (missingArray[0]+1) +
                                     " and " + (missingArray[0]+2) + " is missing");
        }
        else {
            statusLabel = new JLabel(missingSlices + " slices are missing");
        }
        statusLabel.setFont(serif12);

        ButtonGroup sliceGroup = new ButtonGroup();
        average = new JRadioButton("Average", true);
        average.setFont(serif12);
        average.addActionListener(this);
        sliceGroup.add(average); 

        blank = new JRadioButton("Blank", false);
        blank.setFont(serif12);
        blank.addActionListener(this);
        sliceGroup.add(blank);   

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(3, 3, 3, 3);
        
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        slicePanel.add(statusLabel, gbc);

        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.gridwidth = 1;
        slicePanel.add(average, gbc);

        gbc.gridx = 0;
        gbc.gridy = yPos++;
        slicePanel.add(blank, gbc);

//      destination goes in the left of the lower box
        JPanel destinationPanel = new JPanel();
        destinationPanel.setLayout(new BoxLayout(destinationPanel, BoxLayout.Y_AXIS));

        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage); // add the button to the grouping
        destinationPanel.add(newImage); // add the button to the component

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage); // add the button to the grouping
        destinationPanel.add(replaceImage); // add the button to the component

        gbc.gridy = yPos++;
        slicePanel.add(destinationPanel, gbc);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        JPanel buttonPanel = new JPanel();

        /*
         * buildOKButton(); buttonPanel.add(OKButton); buildCancelButton(); buttonPanel.add(cancelButton);
         */
        buttonPanel.add(buildButtons());

        getContentPane().add(slicePanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
        
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        
        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }
        

        if (average.isSelected()) {
            insertBlank = false;
        } 
        else {
            insertBlank = true;
        } 

        return true;
    }
}
