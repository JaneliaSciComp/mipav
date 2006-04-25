package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. The program reduces noise in 3D images by averaging together slices. 3, 5, 7, or all slices can be
 * averaged together. Algorithms are executed in their own thread.
 *
 * @see  AlgorithmSliceAveraging
 */
public class JDialogSliceAveraging extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6973307260738815367L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton allButton;

    /** DOCUMENT ME! */
    private int averagingNumber; // all is 1, other options are 3, 5, and 7

    /** DOCUMENT ME! */
    private int[] destExtents;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private JRadioButton fiveButton;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private ButtonGroup numberGroup;

    /** DOCUMENT ME! */
    private JPanel optionsPanel;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private AlgorithmSliceAveraging sAverAlgo;

    /** DOCUMENT ME! */
    private JRadioButton sevenButton;

    /** DOCUMENT ME! */
    private int sliceNumber;

    /** DOCUMENT ME! */
    private JRadioButton threeButton;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogSliceAveraging() { }

    /**
     * Creates a new JDialogSliceAveraging object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogSliceAveraging(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        image = im;
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogSliceAveraging(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets variables and calls algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if ((source == threeButton) || (source == fiveButton) || (source == sevenButton) ||
                       (source == allButton)) {

            if (allButton.isSelected()) {
                newImage.setSelected(true);
                replaceImage.setSelected(false);
                replaceImage.setEnabled(false);
            } else {

                // Only if the image is unlocked can it be replaced.
                if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
                    replaceImage.setEnabled(true);
                } else {
                    replaceImage.setEnabled(false);
                }
            }
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10023");
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
        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmSliceAveraging) {
            image.clearMask();

            if ((sAverAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        insertScriptLine(algorithm);

        sAverAlgo.finalize();
        sAverAlgo = null;
        dispose();
    }

    /**
     * focusLost - when the user clicks the mouse out of a text field, resets the neccessary variables.
     *
     * @param  event  event that triggers this function
     */
    public void focusLost(FocusEvent event) { }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
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

                userInterface.getScriptDialog().append("SliceAveraging " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");

                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                           " " + averagingNumber + "\n");
                } else {
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                           " " + averagingNumber + "\n");
                }
            }
        }
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged - method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) { }

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

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        if (srcImageKey.equals(destImageKey)) {
            this.setDisplayLocReplace();
        } else {
            this.setDisplayLocNew();
        }

        try {
            setAveragingNumber(parser.getNextInteger());
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
     * Accessor that sets the number of slice that will be averaged together.
     *
     * @param  averagingNumber  DOCUMENT ME!
     */
    public void setAveragingNumber(int averagingNumber) {
        this.averagingNumber = averagingNumber;
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
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    private void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_sliceAveraging");

        if (averagingNumber == 1) { // average all into a 2D image
            destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim
        } else { // averaging number = 3, 5, or 7
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
        }

        if (displayLoc == NEW) {

            try {
                resultImage = new ModelImage(image.getType(), destExtents, name, userInterface);
                resultImage.setImageName(name);

                // Make algorithm
                sAverAlgo = new AlgorithmSliceAveraging(resultImage, image, averagingNumber);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                sAverAlgo.addListener(this);

                // Hide dialog
                setVisible(false);

                if (runInSeparateThread) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (sAverAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    sAverAlgo.setActiveImage(isActiveImage);

                    if (!userInterface.isAppFrameVisible()) {
                        sAverAlgo.setProgressBarVisible(false);
                    }

                    sAverAlgo.run();
                }

            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Slice Averaging: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } // if (displayLoc == NEW)
        else {

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                sAverAlgo = new AlgorithmSliceAveraging(image, averagingNumber);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                sAverAlgo.addListener(this);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (runInSeparateThread) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (sAverAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    sAverAlgo.setActiveImage(isActiveImage);

                    if (!userInterface.isAppFrameVisible()) {
                        sAverAlgo.setProgressBarVisible(false);
                    }

                    sAverAlgo.run();
                }

            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Slice Averaging: unable to allocate enough memory");

                return;
            }
        }

    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {

        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("Slice Averaging");

        JPanel mainPanel;
        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        optionsPanel = new JPanel(new GridBagLayout());
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Slice number"));
        getContentPane().add(optionsPanel);

        sliceNumber = image.getExtents()[2];
        numberGroup = new ButtonGroup();
        threeButton = new JRadioButton("3", false);
        threeButton.setFont(serif12);

        if (sliceNumber < 3) {
            threeButton.setEnabled(false);
        }

        threeButton.addActionListener(this);
        numberGroup.add(threeButton);
        optionsPanel.add(threeButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        fiveButton = new JRadioButton("5", false);
        fiveButton.setFont(serif12);

        if (sliceNumber < 5) {
            fiveButton.setEnabled(false);
        }

        fiveButton.addActionListener(this);
        numberGroup.add(fiveButton);
        optionsPanel.add(fiveButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        sevenButton = new JRadioButton("7", false);
        sevenButton.setFont(serif12);

        if (sliceNumber < 7) {
            sevenButton.setEnabled(false);
        }

        sevenButton.addActionListener(this);
        numberGroup.add(sevenButton);
        optionsPanel.add(sevenButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        allButton = new JRadioButton("All", true);
        allButton.setFont(serif12);
        allButton.addActionListener(this);
        numberGroup.add(allButton);
        optionsPanel.add(allButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(optionsPanel, gbc);

        JPanel outputOptPanel = new JPanel(new GridLayout(1, 1));
        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        outputOptPanel.add(destinationPanel);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage, BorderLayout.NORTH);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        replaceImage.setEnabled(false);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage, BorderLayout.CENTER);

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(outputOptPanel, gbc);

        // JPanel buttonPanel = new JPanel();
        // buildOKButton();
        // buttonPanel.add(OKButton);
        // buildCancelButton();
        // buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        System.gc();

        if (threeButton.isSelected()) {
            averagingNumber = 3;
        } else if (fiveButton.isSelected()) {
            averagingNumber = 5;
        } else if (sevenButton.isSelected()) {
            averagingNumber = 7;
        } else {
            averagingNumber = 1; // allButton.isSelected()
        }

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        return true;
    }

}
