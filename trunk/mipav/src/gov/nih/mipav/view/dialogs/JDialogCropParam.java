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
 * Creates the dialog to crop pixels around the image.
 *
 * <p>User selects:</p>
 *
 * <ol>
 *   <li>Pixels on each side</li>
 *   <li>Pixels on top and bottom</li>
 *   <li>Slices at the front of the image</li>
 *   <li>Slices at the back of the image</li>
 * </ol>
 *
 * A new image or replacement of the old image may be selected.
 */
public class JDialogCropParam extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2434000521271688512L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int back;

    /** DOCUMENT ME! */
    private JTextField backInput;

    /** DOCUMENT ME! */
    private int borderSize = 0;

    /** DOCUMENT ME! */
    private JTextField bottomInput;

    /** DOCUMENT ME! */
    private int bottomSide;

    /** DOCUMENT ME! */
    private AlgorithmCrop cropAlgo;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private int front;

    /** DOCUMENT ME! */
    private JTextField frontInput;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int leftSide;

    /** DOCUMENT ME! */
    private JTextField leftSideInput;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private int rightSide;

    /** DOCUMENT ME! */
    private JTextField rightSideInput;

    /** DOCUMENT ME! */
    private String[] titles; // title of the frame shown when image is NULL

    /** DOCUMENT ME! */
    private JTextField topInput;

    /** DOCUMENT ME! */
    private int topSide;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private int[] xBounds = new int[2];

    /** DOCUMENT ME! */
    private int[] yBounds = new int[2];

    /** DOCUMENT ME! */
    private int[] zBounds = new int[2];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogCropParam() { }

    /**
     * Constructor that makes dialog and displays it.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogCropParam(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ((ViewJFrameImage) parentFrame).getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogCropParam(ViewUserInterface UI, ModelImage im) {
        super();
        image = im;
        parentFrame = image.getParentFrame();
        userInterface = UI;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
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

        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmCrop) {

            if ((cropAlgo.isCompleted() == true) && (resultImage != null)) { // in StoreInDest; "new Image"

                try {
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                insertScriptLine(algorithm);

            } else if ((cropAlgo.isCompleted() == true) && (resultImage == null)) {

                image = cropAlgo.getSrcImage();

                try {
                    imageFrame = new ViewJFrameImage(image, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                insertScriptLine(algorithm);

            } else if (cropAlgo.isCompleted() == false) {

                // algorithm failed but result image still has garbage
                if (resultImage != null) {
                    resultImage.disposeLocal(); // clean up memory
                }

                resultImage = null;

                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if ((((Frame) (imageFrames.elementAt(i))) != parentFrame) && (parentFrame != null)) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }
            }
        }

        cropAlgo.finalize();
        cropAlgo = null;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    public void callAlgorithm() {

        if (displayLoc == NEW) {

            try {
                int[] destExtents = null;

                if (image.getNDims() == 2) {
                    destExtents = new int[2];
                    destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1 + (2 * borderSize);
                    destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1 + (2 * borderSize);
                } else if (image.getNDims() == 3) {

                    if (Math.abs(zBounds[1] - zBounds[0]) == 0) { // crop 3D image to 2D image
                        destExtents = new int[2];
                        destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1 + (2 * borderSize);
                        destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1 + (2 * borderSize);
                    } else {
                        destExtents = new int[3];
                        destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1 + (2 * borderSize);
                        destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1 + (2 * borderSize);
                        destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);
                    }
                } else if (image.getNDims() == 4) {

                    if (Math.abs(zBounds[1] - zBounds[0]) == 0) { // crop 4D image to 3D image
                        destExtents = new int[3];
                        destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1 + (2 * borderSize);
                        destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1 + (2 * borderSize);
                        destExtents[2] = image.getExtents()[3];
                    } else {
                        destExtents = new int[4];
                        destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1 + (2 * borderSize);
                        destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1 + (2 * borderSize);
                        destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);
                        destExtents[3] = image.getExtents()[3];
                    }
                } else {
                    return;
                }

                // Make result image
                resultImage = new ModelImage(image.getType(), destExtents, makeImageName(image.getImageName(),
                                                                                         "_crop"), userInterface);

                cropAlgo = new AlgorithmCrop(resultImage, image, borderSize, xBounds, yBounds, zBounds);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                cropAlgo.addListener(this);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                if (runInSeparateThread) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (cropAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    cropAlgo.setActiveImage(isActiveImage);

                    if (!userInterface.isAppFrameVisible()) {
                        cropAlgo.setProgressBarVisible(false);
                    }

                    cropAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog CropParam: unable to allocate enough memory");

                return;
            }
        } // if (displayLoc == NEW)
        else { // displayLoc == REPLACE

            try {
                cropAlgo = new AlgorithmCrop(image, borderSize, xBounds, yBounds, zBounds);
                cropAlgo.addListener(this);


                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                /*Vector imageFrames = image.getImageFrameVector();
                 *
                 * titles = new String[imageFrames.size()]; for ( int i = 0; i < imageFrames.size(); i++ ) { titles[i] = (
                 * (Frame) ( imageFrames.elementAt( i ) ) ).getTitle(); ( (Frame) ( imageFrames.elementAt( i ) )
                 * ).setTitle( "Locked: " + titles[i] ); ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                 * userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) );}*/

                if (runInSeparateThread) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (cropAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    cropAlgo.setActiveImage(isActiveImage);

                    if (!userInterface.isAppFrameVisible()) {
                        cropAlgo.setProgressBarVisible(false);
                    }

                    cropAlgo.run();
                }

            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog CropParam: unable to allocate enough memory");

                return;
            }

        } // else displayLoc == REPLACE
    }

    /**
     * Accessor that returns the image after adding image margins.
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

                userInterface.getScriptDialog().append("CropParam " +
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

                if (resultImage.getExtents().length == 2) {
                    userInterface.getScriptDialog().append("" + leftSide + " " + rightSide + " " + topSide + " " +
                                                           bottomSide + "\n");
                } else {
                    userInterface.getScriptDialog().append("" + leftSide + " " + rightSide + " " + topSide + " " +
                                                           bottomSide + " " + front + " " + back + "\n");
                }
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

            if (image.getExtents().length == 2) {
                setLeft(parser.getNextInteger());
                setRight(parser.getNextInteger());
                setTop(parser.getNextInteger());
                setBottom(parser.getNextInteger());
            } else {
                setLeft(parser.getNextInteger());
                setRight(parser.getNextInteger());
                setTop(parser.getNextInteger());
                setBottom(parser.getNextInteger());
                setFront(parser.getNextInteger());
                setBack(parser.getNextInteger());
            }
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        xBounds[0] = leftSide;
        xBounds[1] = image.getExtents()[0] - 1 - rightSide;
        yBounds[0] = topSide;
        yBounds[1] = image.getExtents()[1] - 1 - bottomSide;

        if (image.getNDims() >= 3) {
            zBounds[0] = front;
            zBounds[1] = image.getExtents()[2] - 1 - back;
        }

        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
        callAlgorithm();

        if (!srcImageKey.equals(destImageKey)) {
            parser.putVariable(destImageKey, getResultImage().getImageName());
        }
    }

    /**
     * Accessor that sets the back value.
     *
     * @param  x  Value to set back value to.
     */
    public void setBack(int x) {
        back = x;
    }

    /**
     * Accessor that sets the bottom side value.
     *
     * @param  x  Value to set bottom side value to.
     */
    public void setBottom(int x) {
        bottomSide = x;
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
     * Accessor that sets the front value.
     *
     * @param  x  Value to set front value to.
     */
    public void setFront(int x) {
        front = x;
    }

    /**
     * Accessor that sets the left side value.
     *
     * @param  x  Value to set left side value to.
     */
    public void setLeft(int x) {
        leftSide = x;
    }

    /**
     * Accessor that sets the right side value.
     *
     * @param  x  Value to set right side value to.
     */
    public void setRight(int x) {
        rightSide = x;
    }

    /**
     * Accessor that sets the top side value.
     *
     * @param  x  Value to set top side value to.
     */
    public void setTop(int x) {
        topSide = x;
    }

    /**
     * When one of the text inputs has been left blank, trying to convert them to ints results in throwing a null
     * pointer exception. This method determines which one of the JTextFields threw the null pointer Exception.
     *
     * @return  The text field that returned null.
     */
    protected JTextField determineNull() {
        String t;

        try {
            t = rightSideInput.getText();

            if (t.equals("")) {
                return rightSideInput;
            }

            t = leftSideInput.getText();

            if (t.equals("")) {
                return leftSideInput;
            }

            t = topInput.getText();

            if (t.equals("")) {
                return topInput;
            }

            t = bottomInput.getText();

            if (t.equals("")) {
                return bottomInput;
            }

            t = frontInput.getText();

            if (t.equals("")) {
                return frontInput;
            }

            t = backInput.getText();

            if (t.equals("")) {
                return backInput;
            }

            return rightSideInput;
        } catch (NullPointerException npe) {
            MipavUtil.displayError("JDialogCropParam reports: Unknown Error");

            return rightSideInput; // gotta have some thing returned
        }
    }

    /**
     * Initializes the GUI components and places them in the dialog.
     */
    private void init() {
        setTitle("Crop Boundary Pixels");
        setSize(350, 230);
        setForeground(Color.black);

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        JPanel optionPanel = new JPanel();

        // make border
        optionPanel.setBorder(buildTitledBorder("Pixels Around Image"));
        contentBox.add(optionPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        optionPanel.setLayout(gbl);

        gbc.anchor = GridBagConstraints.NORTHWEST;
        // make content, place into layout

        // left
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel leftSideLabel = new JLabel("Pixels on the left side:");
        leftSideLabel.setFont(serif12);
        leftSideLabel.setForeground(Color.black);
        leftSideLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(leftSideLabel, gbc);
        optionPanel.add(leftSideLabel);
        optionPanel.add(Box.createHorizontalStrut(10));

        leftSideInput = new JTextField("0", 4);
        leftSideInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(leftSideInput, false);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(leftSideInput, gbc);
        optionPanel.add(leftSideInput);

        // right
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel rightSideLabel = new JLabel("Pixels on the right side:");
        rightSideLabel.setFont(serif12);
        rightSideLabel.setForeground(Color.black);
        rightSideLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(rightSideLabel, gbc);
        optionPanel.add(rightSideLabel);
        optionPanel.add(Box.createHorizontalStrut(10));

        rightSideInput = new JTextField("0", 4);
        rightSideInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(rightSideInput, false);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(rightSideInput, gbc);
        optionPanel.add(rightSideInput);

        // top
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel topLabel = new JLabel("Pixels on top:");
        topLabel.setFont(serif12);
        topLabel.setForeground(Color.black);
        topLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(topLabel, gbc);
        optionPanel.add(topLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        topInput = new JTextField("0", 4);
        topInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(topInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(topInput, gbc);
        optionPanel.add(topInput);

        // bottom
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel bottomLabel = new JLabel("Pixels on bottom:");
        bottomLabel.setFont(serif12);
        bottomLabel.setForeground(Color.black);
        bottomLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(bottomLabel, gbc);
        optionPanel.add(bottomLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        bottomInput = new JTextField("0", 4);
        bottomInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(bottomInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(bottomInput, gbc);
        optionPanel.add(bottomInput);

        // front
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel frontLabel = new JLabel("Slices at the front of image:");
        frontLabel.setFont(serif12);
        frontLabel.setForeground(Color.black);
        frontLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(frontLabel, gbc);
        optionPanel.add(frontLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        frontInput = new JTextField("0", 4);
        frontInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(frontInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(frontInput, gbc);
        optionPanel.add(frontInput);

        // back
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel backLabel = new JLabel("Slices at the back of image:");
        backLabel.setFont(serif12);
        backLabel.setForeground(Color.black);
        backLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(backLabel, gbc);
        optionPanel.add(backLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        backInput = new JTextField("0", 4);
        backInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(backInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(backInput, gbc);
        optionPanel.add(backInput);

        // image destination select
        JPanel destPanel = new JPanel(); // panel carries no content but box & border
        destPanel.setBorder(buildTitledBorder("Select Destination"));

        Box destinationBox = new Box(BoxLayout.Y_AXIS);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New Image", true);
        newImage.setFont(serif12);
        newImage.addActionListener(this);
        destinationGroup.add(newImage);
        destinationBox.add(newImage);
        newImage.setEnabled(true);

        replaceImage = new JRadioButton("Replace Image", false);
        replaceImage.setFont(serif12);
        replaceImage.addActionListener(this);
        destinationGroup.add(replaceImage);
        destinationBox.add(replaceImage);
        replaceImage.setEnabled(true);
        destPanel.add(destinationBox);
        contentBox.add(destPanel);

        // test speed panel (choice 1: algo has personal buff, imported into img,
        // choice 2: algo calcs where to insert a row.  methinks fewer loops)
        /*
         * JPanel loopingTestPanel = new JPanel(); loopingTestPanel.setBorder(buildTitledBorder("Looping test")); Box
         * loopingTestBox = new Box(BoxLayout.Y_AXIS); loopingGroup = new ButtonGroup(); noBuffer = new JRadioButton("No
         * Buffer", true); noBuffer.setFont(serif12); noBuffer.addActionListener(this); loopingGroup.add(noBuffer);
         * loopingTestBox.add(noBuffer); usingBuffer = new JRadioButton("Uses Buffer", false);
         * usingBuffer.setFont(serif12); usingBuffer.addActionListener(this); loopingGroup.add(usingBuffer);
         * loopingTestBox.add(usingBuffer); loopingTestPanel.add(loopingTestBox); contentBox.add(loopingTestPanel);
         */
        // end looping test display

        /*
         * JPanel OKCancelPanel = new JPanel(new FlowLayout()); OKButton = buildOKButton(); OKCancelPanel.add(OKButton);
         *
         * cancelButton = buildCancelButton(); OKCancelPanel.add(cancelButton); contentBox.add(OKCancelPanel);
         */
        JPanel buttonPanel = new JPanel(new FlowLayout());
        contentBox.add(buildButtons());

        // if this is a 2D image, turn off slice margins
        if (image.getNDims() == 2) {
            frontLabel.setEnabled(false);
            frontInput.setEnabled(false);
            backLabel.setEnabled(false);
            backInput.setEnabled(false);
        }

        mainDialogPanel.add(contentBox);
        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        try {
            rightSide = Integer.parseInt(rightSideInput.getText()); // in pixels
            leftSide = Integer.parseInt(leftSideInput.getText()); // in pixels
            topSide = Integer.parseInt(topInput.getText()); // in pixels
            bottomSide = Integer.parseInt(bottomInput.getText()); // in pixels
            front = Integer.parseInt(frontInput.getText()); // in slices
            back = Integer.parseInt(backInput.getText()); // in slices
        } catch (NumberFormatException nfe) {

            // an empty text-field.  decide which one is empty, then alert the user to correct
            JTextField t = determineNull();
            MipavUtil.displayError("Improper number!");
            t.requestFocus();
            t.selectAll();

            return false;
        }

        if (rightSide < 0) {
            MipavUtil.displayError("Cannot have rightSide < 0");
            rightSideInput.requestFocus();
            rightSideInput.selectAll();

            return false;
        }

        if (rightSide >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have rightSide >= image.getExtents()[0]");
            rightSideInput.requestFocus();
            rightSideInput.selectAll();

            return false;
        }

        if (leftSide < 0) {
            MipavUtil.displayError("Cannot have leftSide < 0");
            leftSideInput.requestFocus();
            leftSideInput.selectAll();

            return false;
        }

        if (leftSide >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have leftSide >= image.getExtents()[0]");
            leftSideInput.requestFocus();
            leftSideInput.selectAll();

            return false;
        }

        if (topSide < 0) {
            MipavUtil.displayError("Cannot have topSide < 0");
            topInput.requestFocus();
            topInput.selectAll();

            return false;
        }

        if (topSide >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have topSide >= image.getExtents()[1]");
            topInput.requestFocus();
            topInput.selectAll();

            return false;
        }

        if (bottomSide < 0) {
            MipavUtil.displayError("Cannot have bottomSide < 0");
            bottomInput.requestFocus();
            bottomInput.selectAll();

            return false;
        }

        if (bottomSide >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have bottomSide >= image.getExtents()[1]");
            bottomInput.requestFocus();
            bottomInput.selectAll();

            return false;
        }


        if (image.getNDims() >= 3) {

            if (front < 0) {
                MipavUtil.displayError("Cannot have front < 0");
                frontInput.requestFocus();
                frontInput.selectAll();

                return false;
            }

            if (front >= image.getExtents()[2]) {
                MipavUtil.displayError("Cannot have front >= image.getExtents()[2]");
                frontInput.requestFocus();
                frontInput.selectAll();

                return false;
            }

            if (back < 0) {
                MipavUtil.displayError("Cannot have back < 0");
                backInput.requestFocus();
                backInput.selectAll();

                return false;
            }

            if (back >= image.getExtents()[2]) {
                MipavUtil.displayError("Cannot have back >= image.getExtents()[2]");
                backInput.requestFocus();
                backInput.selectAll();

                return false;
            }

        } // if (image.getNDims() >= 3)

        if ((image.getExtents()[0] - rightSide - leftSide) <= 0) {
            MipavUtil.displayError("Cannot have image.getExtents()[0] - rightSide - leftSide <= 0");

            return false;
        }

        if ((image.getExtents()[1] - topSide - bottomSide) <= 0) {
            MipavUtil.displayError("Cannot have image.getExtents()[1] - topSide - bottomSide <= 0");

            return false;
        }

        if (image.getNDims() >= 3) {

            if ((image.getExtents()[2] - front - back) <= 0) {
                MipavUtil.displayError("Cannot have image.getExtents()[2] - front - back <= 0");

                return false;
            }
        }

        xBounds[0] = leftSide;
        xBounds[1] = image.getExtents()[0] - 1 - rightSide;
        yBounds[0] = topSide;
        yBounds[1] = image.getExtents()[1] - 1 - bottomSide;

        if (image.getNDims() >= 3) {
            zBounds[0] = front;
            zBounds[1] = image.getExtents()[2] - 1 - back;
        }

        if (newImage.isSelected()) {
            displayLoc = NEW;
        } else {
            displayLoc = REPLACE;
        }

        return true;
    }

}
