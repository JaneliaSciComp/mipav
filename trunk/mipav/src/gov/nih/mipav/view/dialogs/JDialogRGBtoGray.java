package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogRGBtoGray extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3780026821781916357L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float blueValue = 1.0f / 3.0f;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated
                            // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private JRadioButton equalButton;

    /** DOCUMENT ME! */
    private FileInfoBase fInfoBase;

    /** DOCUMENT ME! */
    private JRadioButton graphicsButton;

    /** DOCUMENT ME! */
    private float greenValue = 1.0f / 3.0f;

    /** DOCUMENT ME! */
    private ModelImage imageA = null; // source image

    /** DOCUMENT ME! */
    private boolean intensityAverage;

    /** DOCUMENT ME! */
    private JLabel labelR, labelG, labelB;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** set the values to the defaults -- based on a default of equal values. */
    private float redValue = 1.0f / 3.0f;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private AlgorithmRGBtoGray RGBAlgo;

    /** DOCUMENT ME! */
    private JTextField textR, textG, textB;

    /** DOCUMENT ME! */
    private JTextField textThreshold;

    /** DOCUMENT ME! */
    private float threshold = 0.0f;

    /** DOCUMENT ME! */
    private boolean thresholdAverage;

    /** DOCUMENT ME! */
    private JCheckBox thresholdCheckBox;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private JRadioButton userButton;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRGBtoGray() { }

    /**
     * Sets variables needed to call algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  imA             Source image
     */
    public JDialogRGBtoGray(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, false);
        imageA = imA;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI   The user interface, needed to create the image frame.
     * @param  imA  Source image.
     */
    public JDialogRGBtoGray(ViewUserInterface UI, ModelImage imA) {
        super(false);
        userInterface = UI;
        imageA = imA;
        parentFrame = imageA.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if ((source == equalButton) || (source == graphicsButton) || (source == userButton)) {

            if (equalButton.isSelected()) {
                labelR.setEnabled(false);
                textR.setText("0.3333");
                textR.setEnabled(false);
                labelG.setEnabled(false);
                textG.setText("0.3333");
                textG.setEnabled(false);
                labelB.setEnabled(false);
                textB.setText("0.3333");
                textB.setEnabled(false);
                thresholdCheckBox.setEnabled(true);
            } else if (graphicsButton.isSelected()) {
                labelR.setEnabled(false);
                textR.setText("0.299");
                textR.setEnabled(false);
                labelG.setEnabled(false);
                textG.setText("0.587");
                textG.setEnabled(false);
                labelB.setEnabled(false);
                textB.setText("0.114");
                textB.setEnabled(false);
                thresholdCheckBox.setEnabled(false);
                thresholdCheckBox.setSelected(false);
                textThreshold.setEnabled(false);
            } else {
                labelR.setEnabled(true);
                textR.setText(" ");
                textR.setEnabled(true);
                labelG.setEnabled(true);
                textG.setText(" ");
                textG.setEnabled(true);
                labelB.setEnabled(true);
                textB.setText(" ");
                textB.setEnabled(true);
                thresholdCheckBox.setEnabled(false);
                thresholdCheckBox.setSelected(false);
                textThreshold.setEnabled(false);
            }
        } else if (source == thresholdCheckBox) {

            if (thresholdCheckBox.isSelected()) {
                textThreshold.setEnabled(true);
            } else {
                textThreshold.setEnabled(false);
            }
        } else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10070");
        } else if (command.equals("Cancel")) {
            dispose();
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

        if (algorithm instanceof AlgorithmRGBtoGray) {

            if ((RGBAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                try {
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if ((RGBAlgo.isCompleted() == true) && (resultImage == null)) {

                imageA = RGBAlgo.getSrcImage();

                try {
                    imageFrame = new ViewJFrameImage(imageA, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            // Update frame
            if (parentFrame != null) {
                ((ViewJFrameBase) parentFrame).updateImages(true);
            }

            insertScriptLine(algorithm);
            RGBAlgo.finalize();
            RGBAlgo = null;

        }
    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        if (displayLoc == NEW) {

            try {

                if (imageA.getType() == ModelStorageBase.ARGB) {
                    resultImage = new ModelImage(ModelImage.UBYTE, imageA.getExtents(),
                                                 (imageA.getImageName() + "Gray"), userInterface);
                } else if (imageA.getType() == ModelStorageBase.ARGB_USHORT) {
                    resultImage = new ModelImage(ModelImage.USHORT, imageA.getExtents(),
                                                 (imageA.getImageName() + "Gray"), userInterface);
                } else if (imageA.getType() == ModelStorageBase.ARGB_FLOAT) {
                    resultImage = new ModelImage(ModelImage.FLOAT, imageA.getExtents(),
                                                 (imageA.getImageName() + "Gray"), userInterface);
                }

                // get some important information from imageA and put it in
                // the result image
                for (int n = 0; n < imageA.getFileInfo().length; n++) {
                    fInfoBase = (FileInfoBase) (imageA.getFileInfo(n).clone());
                    fInfoBase.setDataType(resultImage.getType());
                    resultImage.setFileInfo(fInfoBase, n);
                }

                // Make algorithm
                RGBAlgo = new AlgorithmRGBtoGray(resultImage, imageA, redValue, greenValue, blueValue, thresholdAverage,
                                                 threshold, intensityAverage);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                RGBAlgo.addListener(this);

                // Hide dialog
                setVisible(false);

                if (runInSeparateThread) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (RGBAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    RGBAlgo.setActiveImage(isActiveImage);

                    if (!userInterface.isAppFrameVisible()) {
                        RGBAlgo.setProgressBarVisible(false);
                    }

                    RGBAlgo.run();
                }
            } catch (OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                System.gc();
                MipavUtil.displayError("Dialog RGB to Gray: unable to allocate enough memory");

                return;
            }
        } // if (displayLoc == NEW)
        else { // displayLoc == REPLACE

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                // Make algorithm
                RGBAlgo = new AlgorithmRGBtoGray(imageA, redValue, greenValue, blueValue, thresholdAverage, threshold,
                                                 intensityAverage);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                RGBAlgo.addListener(this);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                /*Vector imageFrames = imageA.getImageFrameVector();
                 *
                 * titles = new String[imageFrames.size()]; for ( int i = 0; i < imageFrames.size(); i++ ) { titles[i] = (
                 * (Frame) ( imageFrames.elementAt( i ) ) ).getTitle(); ( (Frame) ( imageFrames.elementAt( i ) )
                 * ).setTitle( "Locked: " + titles[i] ); ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                 * userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) );}*/

                if (runInSeparateThread) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (RGBAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    RGBAlgo.setActiveImage(isActiveImage);

                    if (!userInterface.isAppFrameVisible()) {
                        RGBAlgo.setProgressBarVisible(false);
                    }

                    RGBAlgo.run();
                }

            } catch (OutOfMemoryError x) {
                System.gc();
                MipavUtil.displayError("Dialog RGBtoGRAY: unable to allocate enough memory");

                return;
            }
        } // displayLoc == REPLACE
    }

    /**
     * Accessor that returns the result image.
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

                // check to see if the imageA is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(imageA.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(imageA.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(imageA.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("RGBtoGray " +
                                                       userInterface.getScriptDialog().getVar(imageA.getImageName()) +
                                                       " ");

                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                           " " + redValue + " " + greenValue + " " + blueValue + " " +
                                                           thresholdAverage + " " + threshold + " " + intensityAverage +
                                                           "\n");
                } // if (displayLoc == NEW)
                else { // displayLoc == REPLACE
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(imageA.getImageName()) +
                                                           " " + redValue + " " + greenValue + " " + blueValue + " " +
                                                           thresholdAverage + " " + threshold + " " + intensityAverage +
                                                           "\n");

                } // else displayLoc == REPLACE
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
        imageA = im;
        userInterface = imageA.getUserInterface();
        parentFrame = imageA.getParentFrame();

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

            try {
                setRGBValues(parser.getNextFloat(), parser.getNextFloat(), parser.getNextFloat());
            } catch (Exception e) {
                // Do nothing; it is possible to that a RGBtoGray might not have these values
                // but should still work for legecy scripts.
            }

            setThresholdAverage(parser.getNextBoolean());
            setThreshold(parser.getNextFloat());
            setIntensityAverage(parser.getNextBoolean());
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
     * Sets whether intensity averaging is performed to determine which which channels should be used in calculating the
     * gray.
     *
     * @param  intensityAverage  DOCUMENT ME!
     */
    public void setIntensityAverage(boolean intensityAverage) {
        this.intensityAverage = intensityAverage;

        if (intensityAverage) {
            this.thresholdAverage = false;
        }
    }

    /**
     * Sets the red, green, and blue values to equal values. The values represent the proportion of one color to another
     * (i.e. they must add up to 1.0).
     */
    public void setRGBEqual() {
        redValue = greenValue = blueValue = 1.0f / 3.0f;
    }

    /**
     * Sets the red, green, and blue values to computer graphic values. The values represent the proportion of one color
     * to another (i.e. they must add up to 1.0).
     */
    public void setRGBGraphics() {
        redValue = 0.299f;
        greenValue = 0.587f;
        blueValue = 0.114f;
    }

    /**
     * Sets the red, green, and blue values to the given parameters. The values represent the proportion of one color to
     * another (i.e. they must add up to 1.0).
     *
     * @param  rVal  the red proportion
     * @param  gVal  the green proportion
     * @param  bVal  the blue proportion
     */
    public void setRGBValues(float rVal, float gVal, float bVal) {
        redValue = rVal;
        greenValue = gVal;
        blueValue = bVal;
    }

    /**
     * Sets the threshold for threshold average.
     *
     * @param  threshold  DOCUMENT ME!
     */
    public void setThreshold(float threshold) {
        this.threshold = threshold;
    }

    /**
     * Sets whether threshold averaging is performed - Only include those components greater than threshold.
     *
     * @param  thresholdAverage  DOCUMENT ME!
     */
    public void setThresholdAverage(boolean thresholdAverage) {
        this.thresholdAverage = thresholdAverage;

        if (thresholdAverage) {
            this.intensityAverage = false;
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("RGB -> Gray");
        getContentPane().setLayout(new BorderLayout());

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
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel optionsPanel = new JPanel(new GridLayout(3, 1));
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Weighting methods"));
        mainPanel.add(optionsPanel, gbc);

        ButtonGroup weightGroup = new ButtonGroup();
        equalButton = new JRadioButton("Equal weights", true); // default
        equalButton.setFont(serif12);
        equalButton.addActionListener(this);
        weightGroup.add(equalButton);
        optionsPanel.add(equalButton, BorderLayout.NORTH);

        graphicsButton = new JRadioButton("Computer graphics", false);
        graphicsButton.setFont(serif12);
        graphicsButton.addActionListener(this);
        weightGroup.add(graphicsButton);
        optionsPanel.add(graphicsButton, BorderLayout.CENTER);

        userButton = new JRadioButton("User specified", false);
        userButton.setFont(serif12);
        userButton.addActionListener(this);
        weightGroup.add(userButton);
        optionsPanel.add(userButton, BorderLayout.SOUTH);

        gbc.gridx = 0;
        gbc.gridy = 1;

        JPanel RGBPanel = new JPanel(new GridLayout(4, 2));
        RGBPanel.setForeground(Color.black);
        RGBPanel.setBorder(buildTitledBorder("RGB weight factors"));
        mainPanel.add(RGBPanel, gbc);

        labelR = new JLabel("Red");
        labelR.setForeground(Color.black);
        labelR.setFont(serif12);
        labelR.setEnabled(false);
        RGBPanel.add(labelR);
        textR = new JTextField();
        textR.setText("0.3333");
        textR.setFont(serif12);
        textR.setEnabled(false);
        RGBPanel.add(textR);

        labelG = new JLabel("Green");
        labelG.setForeground(Color.black);
        labelG.setFont(serif12);
        labelG.setEnabled(false);
        RGBPanel.add(labelG);
        textG = new JTextField();
        textG.setText("0.3333");
        textG.setFont(serif12);
        textG.setEnabled(false);
        RGBPanel.add(textG);

        labelB = new JLabel("Blue");
        labelB.setForeground(Color.black);
        labelB.setFont(serif12);
        labelB.setEnabled(false);
        RGBPanel.add(labelB);
        textB = new JTextField();
        textB.setText("0.3333");
        textB.setFont(serif12);
        textB.setEnabled(false);
        RGBPanel.add(textB);

        thresholdCheckBox = new JCheckBox("Only average RGB values greater than");
        thresholdCheckBox.setFont(serif12);
        thresholdCheckBox.setForeground(Color.black);
        thresholdCheckBox.setEnabled(true);
        thresholdCheckBox.setSelected(false);
        thresholdCheckBox.addActionListener(this);
        RGBPanel.add(thresholdCheckBox);

        textThreshold = new JTextField();
        textThreshold.setText("0");
        textThreshold.setFont(serif12);
        textThreshold.setEnabled(false);
        RGBPanel.add(textThreshold);

        gbc.gridx = 0;
        gbc.gridy = 2;
        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        mainPanel.add(destinationPanel, gbc);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage, BorderLayout.NORTH);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage, BorderLayout.CENTER);

        // Only if the image is unlocked can it be replaced.
        if (imageA.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }


        JPanel buttonPanel = new JPanel();

        /*
         * buildOKButton(); buttonPanel.add(OKButton); buildCancelButton(); buttonPanel.add(cancelButton);
         */
        buttonPanel.add(buildButtons());

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
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
        String tmpStr;
        float minValue, maxValue;

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if (equalButton.isSelected()) {
            redValue = greenValue = blueValue = 1.0f / 3.0f;
            thresholdAverage = thresholdCheckBox.isSelected();

            if (thresholdAverage) {
                tmpStr = textThreshold.getText();

                if (imageA.getType() == ModelImage.ARGB) {
                    minValue = 0.0f;
                    maxValue = 255.0f;
                } else if (imageA.getType() == ModelImage.ARGB_USHORT) {
                    minValue = 0.0f;
                    maxValue = 65535.0f;
                } else {
                    minValue = -Float.MAX_VALUE;
                    maxValue = Float.MAX_VALUE;
                }

                if (testParameter(tmpStr, minValue, maxValue)) {
                    threshold = Float.valueOf(tmpStr).floatValue();
                } else {
                    textThreshold.requestFocus();
                    textThreshold.selectAll();

                    return false;
                }
            } // if (thresholdAverage)

            return true;
        } // if (equalButton.isSelected())

        if (graphicsButton.isSelected()) {
            redValue = 0.299f;
            greenValue = 0.587f;
            blueValue = 0.114f;

            return true;
        }

        tmpStr = textR.getText();

        if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
            redValue = Float.valueOf(tmpStr).floatValue();
        } else {
            textR.requestFocus();
            textR.selectAll();

            return false;
        }

        tmpStr = textG.getText();

        if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
            greenValue = Float.valueOf(tmpStr).floatValue();
        } else {
            textG.requestFocus();
            textG.selectAll();

            return false;
        }

        tmpStr = textB.getText();

        if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
            blueValue = Float.valueOf(tmpStr).floatValue();
        } else {
            textB.requestFocus();
            textB.selectAll();

            return false;
        }

        return true;
    }
}
