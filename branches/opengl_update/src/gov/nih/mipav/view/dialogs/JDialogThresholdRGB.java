package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * <p>Title: JDialogThresholdRGB</p>
 *
 * <p>Description: dialog for running RGB threshold algorithm on color images</p>
 *
 * <p>Copyright: Copyright (c) 2003</p>
 *
 * <p>Company:</p>
 *
 * @author   linkb
 * @version  1.0
 */
public class JDialogThresholdRGB extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1903960650432668714L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** false = apply algorithm only to VOI regions apply same threshold to all. */
    private JCheckBox applyToAllBox;

    /** enable blue threshold. */
    private JCheckBox applyToBlueBox;

    /** enable green threshold. */
    private JCheckBox applyToGreenBox;

    /** enable red threshold. */
    private JCheckBox applyToRedBox;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private float[] fillValues;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** choose inverse or no inverse threshold. */
    private JCheckBox inverseOptionBox;

    /** inverse? */
    private boolean isInverse = false;

    /** DOCUMENT ME! */
    private float[] maxs = new float[3];

    /** Minimum and maximum values for red/gree/blue. */
    private float[] mins = new float[3];

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** or if the source image is to be replaced. */
    private boolean regionFlag; // true = apply algorithm to the whole image

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField[] textFills;

    /** Text fields for lower & upper thresholds and fill values. */
    private JTextField[] textThreshold1;

    /** DOCUMENT ME! */
    private JTextField[] textThreshold2;

    /** DOCUMENT ME! */
    private AlgorithmThresholdDualRGB thresholdAlgoRGB;

    /** DOCUMENT ME! */
    private float[] thresholdB;

    /** DOCUMENT ME! */
    private float[] thresholdG;

    /** DOCUMENT ME! */
    private JLabel[] thresholdLabels1;

    /** DOCUMENT ME! */
    private JLabel[] thresholdLabels2;

    /** lower [0] and upper [1] thresholds for RGB. */
    private float[] thresholdR;

    /** DOCUMENT ME! */
    private String[] titles;

    /** tells whether or not to use the channels (red=0, green=1, blue=2). */
    private boolean[] useChannel = new boolean[3];

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogThresholdRGB() { }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  im  Source image.
     */
    public JDialogThresholdRGB(ModelImage im) {
        super();
        userInterface = ViewUserInterface.getReference();
        image = im;
        parentFrame = image.getParentFrame();
    }

    /**
     * Creates a new JDialogThresholdRGB object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogThresholdRGB(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();

        this.setResizable(false);
        setVisible(true);
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
        }  else if (command.equals("Help")) {
            //MipavUtil.showHelp("19074");
            MipavUtil.showWebHelp("Threshold");
        } else {
            super.actionPerformed(event);
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

        if (algorithm instanceof AlgorithmThresholdDualRGB) {
            image.clearMask();

            if ((thresholdAlgoRGB.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

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

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        thresholdAlgoRGB.finalize();
        thresholdAlgoRGB = null;
        dispose();
    }

    // *******************************************************************
    // ************************* Focus Events ****************************
    // *******************************************************************

    /**
     * When the user clicks the mouse out of a text field, resets the necessary variables. In this case, it also handles
     * a bug involving when the focusGained and focusLost functions are called by turning off the ability of the text
     * fields to call those functions.
     *
     * @param  event  Event that triggers this function.
     */
    public void focusLost(FocusEvent event) {
    	
        /*int panelNum = -1;
        JTextField field = (JTextField) event.getSource();

        if (field == textThreshold1[0]) {
            panelNum = 0;
        } else if (field == textThreshold1[1]) {
            panelNum = 1;
        } else if (field == textThreshold1[2]) {
            panelNum = 2;
        } else {
            return;
        }*/
        /*
         * String text = field.getText(); String tempStr;
         *
         * Double stringConv = new Double(0);
         *
         * field.removeFocusListener(this);
         *
         * if (testParameter(text, mins[panelNum], maxs[panelNum])) { tempStr = new String("Upper limit: ( " +
         * makeString(Float.parseFloat(text),3) + " - " +                      makeString(maxs[panelNum],3) + " ).");
         * thresholdLabels2[panelNum].setText(tempStr); thres1 = stringConv.valueOf(text).floatValue(); text =
         * textThreshold2[panelNum].getText(); thres2 = stringConv.valueOf(text).floatValue(); if (thres2 < thres1) {
         * thres2 = (thres1 + maxs[panelNum]) / 2;     textThreshold2[panelNum].setText(String.valueOf(thres2)); } }
         * else { field.requestFocus(); field.selectAll(); field.addFocusListener(this); return; }
         *
         * field.addFocusListener(this);
         */
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Monitors the checkboxes and handles events according (enable/disable).
     *
     * @param  event  ItemEvent
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source.equals(applyToAllBox)) {

            if (applyToAllBox.isSelected()) {
                textThreshold1[0].setEnabled(true);
                textThreshold2[0].setEnabled(true);
                textThreshold1[1].setEnabled(false);
                textThreshold1[2].setEnabled(false);
                textThreshold2[1].setEnabled(false);
                textThreshold2[2].setEnabled(false);
                textFills[0].setEnabled(true);
                textFills[1].setEnabled(false);
                textFills[2].setEnabled(false);

                textThreshold1[1].setText(textThreshold1[0].getText());
                textThreshold1[2].setText(textThreshold1[0].getText());
                textThreshold2[1].setText(textThreshold1[0].getText());
                textThreshold2[2].setText(textThreshold1[0].getText());
                textFills[1].setText(textFills[0].getText());
                textFills[2].setText(textFills[0].getText());

                applyToRedBox.setEnabled(false);
                applyToGreenBox.setEnabled(false);
                applyToBlueBox.setEnabled(false);

            } else {
                textThreshold1[0].setEnabled(applyToRedBox.isSelected());
                textThreshold1[1].setEnabled(applyToGreenBox.isSelected());
                textThreshold1[2].setEnabled(applyToBlueBox.isSelected());

                textThreshold2[0].setEnabled(applyToRedBox.isSelected());
                textThreshold2[1].setEnabled(applyToGreenBox.isSelected());
                textThreshold2[2].setEnabled(applyToBlueBox.isSelected());

                textFills[0].setEnabled(applyToRedBox.isSelected());
                textFills[1].setEnabled(applyToGreenBox.isSelected());
                textFills[2].setEnabled(applyToBlueBox.isSelected());

                applyToRedBox.setEnabled(true);
                applyToGreenBox.setEnabled(true);
                applyToBlueBox.setEnabled(true);
            }
        } else if (source.equals(applyToRedBox)) {

            if (applyToRedBox.isSelected()) {
                textThreshold1[0].setEnabled(true);
                textThreshold2[0].setEnabled(true);
                textFills[0].setEnabled(true);
            } else {
                textThreshold1[0].setEnabled(false);
                textThreshold2[0].setEnabled(false);
                textFills[0].setEnabled(false);
            }
        } else if (source.equals(applyToGreenBox)) {

            if (applyToGreenBox.isSelected()) {
                textThreshold1[1].setEnabled(true);
                textThreshold2[1].setEnabled(true);
                textFills[1].setEnabled(true);
            } else {
                textThreshold1[1].setEnabled(false);
                textThreshold2[1].setEnabled(false);
                textFills[1].setEnabled(false);
            }
        } else if (source.equals(applyToBlueBox)) {

            if (applyToBlueBox.isSelected()) {
                textThreshold1[2].setEnabled(true);
                textThreshold2[2].setEnabled(true);
                textFills[2].setEnabled(true);
            } else {
                textThreshold1[2].setEnabled(false);
                textThreshold2[2].setEnabled(false);
                textFills[2].setEnabled(false);
            }
        }
    }

    /**
     * Function to run the threshold algorithm from the ViewJFrameHistoRGB.
     *
     * @param  im         ModelImage the image
     * @param  red        float[] red threshold
     * @param  green      float[] green threshold
     * @param  blue       float[] blue threshold
     * @param  fillV      float[] fill values
     * @param  isInverse  boolean (inverse threshold or not)
     */
    public void runFromLUTFrame(ModelImage im, float[] red, float[] green, float[] blue, float[] fillV,
                                boolean isInverse) {
        this.image = im;
        this.userInterface = ViewUserInterface.getReference();

        this.thresholdR = red;
        this.thresholdG = green;
        this.thresholdB = blue;
        this.fillValues = fillV;
        this.isInverse = isInverse;

        useChannel[0] = (thresholdR != null);
        useChannel[1] = (thresholdG != null);
        useChannel[2] = (thresholdB != null);

        regionFlag = true;
        setDisplayLocNew();
        setSeparateThread(true);
        callAlgorithm();
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
     * Accessor that sets the fill value. Will not be used if writing out a binary image.
     *
     * @param  scale  Value to set fill value to.
     */
    public void setFillValues(float[] scale) {
        fillValues = scale;
    }

    /**
     * Accessor that sets the region flag.
     *
     * @param  flag  <code>true</code> indicates the whole image is blurred, <code>false</code> indicates a region.
     */
    public void setRegionFlag(boolean flag) {
        regionFlag = flag;
    }

    /**
     * Sets the blue threshold.
     *
     * @param  thresB  float[]
     */
    public void setThresholdB(float[] thresB) {
        this.thresholdB = thresB;
    }

    /**
     * Sets the green threshold.
     *
     * @param  thresG  float[]
     */
    public void setThresholdG(float[] thresG) {
        this.thresholdG = thresG;
    }

    /**
     * Sets the red threshold (float[2]... 1st is lower, 2nd is upper)
     *
     * @param  thresR  float[]
     */
    public void setThresholdR(float[] thresR) {
        this.thresholdR = thresR;
    }

    /**
     * Once all the necessary variables are set, call the threshold rgb algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        String name = makeImageName(image.getImageName(), "_threshold");
        int[] destExtents = null;

        int end;

        if (image.getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim
            end = 1;
        } else if (image.getNDims() == 3) {
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
            end = destExtents[2];
        } else { // Dims = 4
            destExtents = new int[4];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
            destExtents[3] = image.getExtents()[3];
            end = destExtents[2] * destExtents[3];
        }

        if (displayLoc == NEW) {

            try {

                // resultImage      = new ModelImage(image.getType(), destExtents, " Threshold", userInterface);
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                    for (int i = 0; i < end; i++) {
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                    }
                }

                // Make algorithm
                thresholdAlgoRGB = new AlgorithmThresholdDualRGB(resultImage, image, thresholdR, thresholdG, thresholdB,
                                                                 fillValues, useChannel, regionFlag, isInverse);
                thresholdAlgoRGB.addListener(this);

                createProgressBar(image.getImageName(), thresholdAlgoRGB);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (thresholdAlgoRGB.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    thresholdAlgoRGB.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog threshold: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else {

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                thresholdAlgoRGB = new AlgorithmThresholdDualRGB(image, thresholdR, thresholdG, thresholdB, fillValues,
                                                                 useChannel, regionFlag, isInverse);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                thresholdAlgoRGB.addListener(this);

                createProgressBar(image.getImageName(), thresholdAlgoRGB);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (thresholdAlgoRGB.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    thresholdAlgoRGB.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog threshold: unable to allocate enough memory");

                return;
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(resultImage);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            displayLoc = NEW;
        } else {
            displayLoc = REPLACE;
        }

        regionFlag = scriptParameters.doProcessWholeImage();
        useChannel = scriptParameters.getParams().getList("useChannel").getAsBooleanArray();

        fillValues = new float[3];

        if (useChannel[0]) {
            thresholdR = scriptParameters.getParams().getList("thresholdR").getAsFloatArray();
            fillValues[0] = scriptParameters.getParams().getFloat("fillValues[0]");
        }

        if (useChannel[1]) {
            thresholdG = scriptParameters.getParams().getList("thresholdG").getAsFloatArray();
            fillValues[1] = scriptParameters.getParams().getFloat("fillValues[1]");
        }

        if (useChannel[1]) {
            thresholdB = scriptParameters.getParams().getList("thresholdB").getAsFloatArray();
            fillValues[2] = scriptParameters.getParams().getFloat("fillValues[2]");
        }

        isInverse = scriptParameters.getParams().getBoolean("isInverse");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, displayLoc == NEW);


        scriptParameters.storeProcessWholeImage(regionFlag);
        scriptParameters.getParams().put(ParameterFactory.newParameter("useChannel", useChannel));

        if (useChannel[0]) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("thresholdR", thresholdR));
            scriptParameters.getParams().put(ParameterFactory.newParameter("fillValues[0]", fillValues[0]));
        }

        if (useChannel[1]) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("thresholdG", thresholdG));
            scriptParameters.getParams().put(ParameterFactory.newParameter("fillValues[1]", fillValues[1]));
        }

        if (useChannel[1]) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("thresholdB", thresholdB));
            scriptParameters.getParams().put(ParameterFactory.newParameter("fillValues[2]", fillValues[2]));
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("isInverse", isInverse));
    }

    /**
     * Builds each threshold panel (red, green, and blue).
     *
     * @param   panelNum  int 0-2
     *
     * @return  JPanel
     */
    private JPanel buildThresholdPanel(int panelNum) {
        String tempStr;
        JPanel panel = new JPanel(new GridBagLayout());

        if (panelNum == 0) {
            panel.setBorder(buildTitledBorder("Red"));
        } else if (panelNum == 1) {
            panel.setBorder(buildTitledBorder("Green"));
        } else if (panelNum == 2) {
            panel.setBorder(buildTitledBorder("Blue"));
        }

        tempStr = new String("Lower limit: ( " + makeString(mins[panelNum], 2) + " - " + makeString(maxs[panelNum], 2) +
                             " )");
        thresholdLabels1[panelNum] = new JLabel(tempStr);
        thresholdLabels1[panelNum].setForeground(Color.black);
        thresholdLabels1[panelNum].setFont(serif12);

        textThreshold1[panelNum] = new JTextField(4);
        textThreshold1[panelNum].setText(makeString((maxs[panelNum] + mins[panelNum]) / 3, 2));
        textThreshold1[panelNum].setFont(serif12);
        textThreshold1[panelNum].setCaretPosition(0);
        textThreshold1[panelNum].addFocusListener(this);

        tempStr = new String("Upper limit: ( [Lower limit] - " + makeString(maxs[panelNum], 2) + " )");

        thresholdLabels2[panelNum] = new JLabel(tempStr);
        thresholdLabels2[panelNum].setForeground(Color.black);
        thresholdLabels2[panelNum].setFont(serif12);

        textThreshold2[panelNum] = new JTextField(4);
        textThreshold2[panelNum].setText(makeString((maxs[panelNum] + mins[panelNum]) / 1.5f, 2));
        textThreshold2[panelNum].setFont(serif12);
        textThreshold2[panelNum].setCaretPosition(0);
        textThreshold2[panelNum].addFocusListener(this);

        JLabel labelFill = new JLabel("Set outside values to: ");
        labelFill.setForeground(Color.black);
        labelFill.setFont(serif12);

        textFills[panelNum] = new JTextField(4);
        textFills[panelNum].setText(makeString(mins[panelNum], 2));
        textFills[panelNum].setFont(serif12);
        textFills[panelNum].setCaretPosition(0);
        textFills[panelNum].setEnabled(true);
        textFills[panelNum].addFocusListener(this);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);
        gbc.fill = GridBagConstraints.NONE;
        panel.add(thresholdLabels1[panelNum], gbc);

        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx = 1;
        panel.add(textThreshold1[panelNum], gbc);

        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy++;

        gbc.anchor = GridBagConstraints.WEST;
        panel.add(thresholdLabels2[panelNum], gbc);

        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx = 1;
        panel.add(textThreshold2[panelNum], gbc);

        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy++;

        gbc.gridwidth = 1;
        panel.add(labelFill, gbc);

        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx = 1;
        panel.add(textFills[panelNum], gbc);

        return panel;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Threshold");

        mins[0] = (float) image.getMinR();
        maxs[0] = (float) image.getMaxR();
        mins[1] = (float) image.getMinG();
        maxs[1] = (float) image.getMaxG();
        mins[2] = (float) image.getMinB();
        maxs[2] = (float) image.getMaxB();


        thresholdR = new float[2];
        thresholdG = new float[2];
        thresholdB = new float[2];
        fillValues = new float[3];

        textThreshold1 = new JTextField[3];
        textThreshold2 = new JTextField[3];

        thresholdLabels1 = new JLabel[3];
        thresholdLabels2 = new JLabel[3];

        textFills = new JTextField[3];

        JPanel thresholdPanel = new JPanel();
        thresholdPanel.setLayout(new BoxLayout(thresholdPanel, BoxLayout.Y_AXIS));
        thresholdPanel.setBorder(buildTitledBorder("Thresholds"));

        GridBagConstraints gbc2 = new GridBagConstraints();
        JPanel applyPanel = new JPanel();
        applyPanel.setLayout(new GridBagLayout());
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.weightx = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.fill = GridBagConstraints.NONE;

        applyToAllBox = new JCheckBox("Apply same threshold to all channels");
        applyToAllBox.addItemListener(this);
        applyToAllBox.setFont(MipavUtil.font12);

        applyToRedBox = new JCheckBox("Use red channel");
        applyToRedBox.setSelected(true);
        applyToRedBox.addItemListener(this);
        applyToRedBox.setFont(MipavUtil.font12);

        applyToGreenBox = new JCheckBox("Use green channel");
        applyToGreenBox.setSelected(true);
        applyToGreenBox.addItemListener(this);
        applyToGreenBox.setFont(MipavUtil.font12);

        applyToBlueBox = new JCheckBox("Use blue channel");
        applyToBlueBox.setSelected(true);
        applyToBlueBox.addItemListener(this);
        applyToBlueBox.setFont(MipavUtil.font12);

        inverseOptionBox = new JCheckBox("Use inverse threshold");
        inverseOptionBox.addItemListener(this);
        inverseOptionBox.setFont(MipavUtil.font12);

        applyPanel.setBorder(buildTitledBorder(""));
        applyPanel.add(applyToAllBox, gbc2);

        gbc2.gridy = 1;
        applyPanel.add(applyToRedBox, gbc2);

        gbc2.gridy = 2;
        applyPanel.add(applyToGreenBox, gbc2);

        gbc2.gridy = 3;
        applyPanel.add(applyToBlueBox, gbc2);

        gbc2.gridy = 4;
        applyPanel.add(inverseOptionBox, gbc2);

        thresholdPanel.add(applyPanel);

        for (int i = 0; i < 3; i++) {
            thresholdPanel.add(buildThresholdPanel(i));
        }

        GridBagConstraints gbc = new GridBagConstraints();

        JPanel destinationPanel = new JPanel(new GridBagLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setBounds(10, 42, 120, 25);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        destinationPanel.add(newImage, gbc);
        gbc.gridy = 1;
        destinationPanel.add(replaceImage, gbc);

        JPanel imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Threshold"));

        ButtonGroup imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);

        gbc.gridx = 0;
        gbc.gridy = 0;
        imageVOIPanel.add(wholeImage, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(VOIRegions, gbc);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        mainPanel.add(thresholdPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        mainPanel.add(destinationPanel, gbc);
        gbc.gridx = 1;
        mainPanel.add(imageVOIPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildButtons());

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr, tmpStr2;

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if (wholeImage.isSelected()) {
            regionFlag = true;
        } else if (VOIRegions.isSelected()) {
            regionFlag = false;
        }

        if (applyToAllBox.isSelected()) {
            tmpStr = textThreshold1[0].getText();

            if (testParameter(tmpStr, image.getMin(), image.getMax())) {
                thresholdR[0] = thresholdG[0] = thresholdB[0] = Float.valueOf(tmpStr).floatValue();
            } else {
                textThreshold1[0].requestFocus();
                textThreshold1[0].selectAll();

                return false;
            }

            tmpStr2 = textThreshold2[0].getText();

            if (testParameter(tmpStr2, Float.valueOf(tmpStr).floatValue(), image.getMax())) {
                thresholdR[1] = thresholdG[1] = thresholdB[1] = Float.valueOf(tmpStr2).floatValue();
            } else {
                textThreshold2[0].requestFocus();
                textThreshold2[0].selectAll();

                return false;
            }

            tmpStr = textFills[0].getText();

            if (testParameter(tmpStr, image.getMin(), image.getMax())) {
                fillValues[0] = fillValues[1] = fillValues[2] = Float.valueOf(tmpStr).floatValue();
            }

            useChannel[0] = useChannel[1] = useChannel[2] = true;

        } else {
            float tempThres = 0f;

            // test the lower threshold, upper threshold, and fill values
            for (int i = 0; i < 3; i++) {

                if (textThreshold1[i].isEnabled()) {
                    tmpStr = textThreshold1[i].getText();

                    if (testParameter(tmpStr, mins[i], maxs[i])) {

                        switch (i) {

                            case 0:
                                thresholdR[0] = Float.valueOf(tmpStr).floatValue();
                                break;

                            case 1:
                                thresholdG[0] = Float.valueOf(tmpStr).floatValue();
                                break;

                            case 2:
                                thresholdB[0] = Float.valueOf(tmpStr).floatValue();
                                break;
                        }
                    } else {
                        textThreshold1[i].requestFocus();
                        textThreshold1[i].selectAll();

                        return false;
                    }

                    tmpStr = textThreshold2[i].getText();

                    switch (i) {

                        case 0:
                            tempThres = thresholdR[0];
                            break;

                        case 1:
                            tempThres = thresholdG[0];
                            break;

                        case 2:
                            tempThres = thresholdB[0];
                            break;
                    }

                    if (testParameter(tmpStr, tempThres, maxs[i])) {

                        switch (i) {

                            case 0:
                                thresholdR[1] = Float.valueOf(tmpStr).floatValue();
                                break;

                            case 1:
                                thresholdG[1] = Float.valueOf(tmpStr).floatValue();
                                break;

                            case 2:
                                thresholdB[1] = Float.valueOf(tmpStr).floatValue();
                                break;
                        }
                    } else {
                        textThreshold2[i].requestFocus();
                        textThreshold2[i].selectAll();

                        return false;
                    }

                    tmpStr = textFills[i].getText();

                    if (testParameter(tmpStr, mins[i], maxs[i])) {
                        fillValues[i] = Float.valueOf(tmpStr).floatValue();
                    } else {
                        textFills[i].requestFocus();
                        textFills[i].selectAll();

                        return false;
                    }

                    useChannel[i] = true;
                } else {
                    useChannel[i] = false;
                }
            }
        }

        this.isInverse = inverseOptionBox.isSelected();

        return true;
    }

}
