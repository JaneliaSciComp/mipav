package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * <p>Title: JDialogCenterOfMassRGB</p>
 *
 * <p>Description: dialog for running RGB center of mass calculations on color images</p>
 *
 * <p>Copyright: Copyright (c) 2008</p>
 *
 * <p>Company:</p>
 *
 * @author   William Gandler
 * @version  1.0
 */
public class JDialogCenterOfMassRGB extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** false = apply algorithm only to VOI regions apply same threshold to all. */
    private JCheckBox applyToAllBox;
   
    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private float[] maxs = new float[3];

    /** Minimum and maximum values for red/gree/blue. */
    private float[] mins = new float[3];

    /** or if the source image is to be replaced. */
    private boolean regionFlag; // true = apply algorithm to the whole image

    /** Text fields for lower & upper thresholds and fill values. */
    private JTextField[] textThreshold1;

    /** DOCUMENT ME! */
    private JTextField[] textThreshold2;

    /** DOCUMENT ME! */
    private AlgorithmCenterOfMassRGB comAlgoRGB;

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
    public JDialogCenterOfMassRGB() { }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  im  Source image.
     */
    public JDialogCenterOfMassRGB(ModelImage im) {
        super();
        userInterface = ViewUserInterface.getReference();
        image = im;
        parentFrame = image.getParentFrame();
    }

    /**
     * Creates a new JDialogCenterOfMassRGB object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogCenterOfMassRGB(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();

        //this.setResizable(false);
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

        if (algorithm instanceof AlgorithmCenterOfMassRGB) {
            image.clearMask();

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
            
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        comAlgoRGB.finalize();
        comAlgoRGB = null;
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
        Object source = event.getSource();
        JTextField field = (JTextField) event.getSource();

        if (field == textThreshold1[0]) {
            panelNum = 0;
        } else if (field == textThreshold1[1]) {
            panelNum = 1;
        } else if (field == textThreshold1[2]) {
            panelNum = 2;
        } else {
            return;
        }
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

                textThreshold1[1].setText(textThreshold1[0].getText());
                textThreshold1[2].setText(textThreshold1[0].getText());
                textThreshold2[1].setText(textThreshold1[0].getText());
                textThreshold2[2].setText(textThreshold1[0].getText());

            } else {
                textThreshold1[0].setEnabled(true);
                textThreshold1[1].setEnabled(true);
                textThreshold1[2].setEnabled(true);

                textThreshold2[0].setEnabled(true);
                textThreshold2[1].setEnabled(true);
                textThreshold2[2].setEnabled(true);

            }
        } 
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
        try {

            // No need to make new image space because the user has choosen to replace the source image
            // Make the algorithm class
            comAlgoRGB = new AlgorithmCenterOfMassRGB(image, thresholdR, thresholdG, thresholdB,
                                                             regionFlag);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            comAlgoRGB.addListener(this);

            createProgressBar(image.getImageName(), comAlgoRGB);

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
                if (comAlgoRGB.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                comAlgoRGB.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Center Of Mass RGB: unable to allocate enough memory");

            return;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        regionFlag = scriptParameters.doProcessWholeImage();

        thresholdR = scriptParameters.getParams().getList("thresholdR").getAsFloatArray();
        thresholdG = scriptParameters.getParams().getList("thresholdG").getAsFloatArray();
        thresholdB = scriptParameters.getParams().getList("thresholdB").getAsFloatArray();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.storeProcessWholeImage(regionFlag);

        scriptParameters.getParams().put(ParameterFactory.newParameter("thresholdR", thresholdR));
        scriptParameters.getParams().put(ParameterFactory.newParameter("thresholdG", thresholdG));
        scriptParameters.getParams().put(ParameterFactory.newParameter("thresholdB", thresholdB));
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
        textThreshold1[panelNum].setText(makeString(mins[panelNum], 2));
        textThreshold1[panelNum].setFont(serif12);
        textThreshold1[panelNum].setCaretPosition(0);
        textThreshold1[panelNum].addFocusListener(this);

        tempStr = new String("Upper limit: ( [Lower limit] - " + makeString(maxs[panelNum], 2) + " )");

        thresholdLabels2[panelNum] = new JLabel(tempStr);
        thresholdLabels2[panelNum].setForeground(Color.black);
        thresholdLabels2[panelNum].setFont(serif12);

        textThreshold2[panelNum] = new JTextField(4);
        textThreshold2[panelNum].setText(makeString(maxs[panelNum], 2));
        textThreshold2[panelNum].setFont(serif12);
        textThreshold2[panelNum].setCaretPosition(0);
        textThreshold2[panelNum].addFocusListener(this);

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
        return panel;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Center of Mass");

        mins[0] = (float) image.getMinR();
        maxs[0] = (float) image.getMaxR();
        mins[1] = (float) image.getMinG();
        maxs[1] = (float) image.getMaxG();
        mins[2] = (float) image.getMinB();
        maxs[2] = (float) image.getMaxB();


        thresholdR = new float[2];
        thresholdG = new float[2];
        thresholdB = new float[2];

        textThreshold1 = new JTextField[3];
        textThreshold2 = new JTextField[3];

        thresholdLabels1 = new JLabel[3];
        thresholdLabels2 = new JLabel[3];

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

        applyPanel.setBorder(buildTitledBorder(""));
        applyPanel.add(applyToAllBox, gbc2);

        thresholdPanel.add(applyPanel);

        for (int i = 0; i < 3; i++) {
            thresholdPanel.add(buildThresholdPanel(i));
        }

        GridBagConstraints gbc = new GridBagConstraints();

        JPanel imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Process"));

        ButtonGroup imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        imageVOIPanel.add(wholeImage, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(VOIRegions, gbc);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        mainPanel.add(thresholdPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(imageVOIPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

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

        } else { // applyToAllBox not selected
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

                } 
            } // for (int i = 0; i < 3; i++)
        } // else applyToAllBox not selected

        return true;
    }

}
