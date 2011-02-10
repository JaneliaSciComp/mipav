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
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */
public class JDialogXcosmEM extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 981523715011352299L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int percentIterationsHalfSize = 0;

    /** DOCUMENT ME! */
    int percentIterationsOriginalSize = 100;

    /** DOCUMENT ME! */
    int percentIterationsQuarterSize = 0;

    /** DOCUMENT ME! */
    AlgorithmXcosmEM xcosmEMAlgo;

    /** DOCUMENT ME! */
    private int backupNumberIterations = 50;

    /** DOCUMENT ME! */
    private float decayConstant = 1.0f;

    /** DOCUMENT ME! */
    private boolean entireImage = true;

    /** DOCUMENT ME! */
    private boolean estimateDecay = false;

    /** DOCUMENT ME! */
    private JComboBox imageComboBox;

    /** DOCUMENT ME! */
    private JLabel labelImage;

    /** DOCUMENT ME! */
    private ModelImage originalImage;

    /** DOCUMENT ME! */
    private boolean penaltyIntensity = true;

    /** DOCUMENT ME! */
    private float penaltyValue = 0.0f;

    /** DOCUMENT ME! */
    private ModelImage psfImage = null;

    /** DOCUMENT ME! */
    private JRadioButton radioEntireImage, radioVOIRegion;

    /** DOCUMENT ME! */
    private JRadioButton radioEstimateDecay;

    /** DOCUMENT ME! */
    private JRadioButton radioIntensity, radioRoughness;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textBackupNumberIterations;

    /** DOCUMENT ME! */
    private JTextField textDecayConstant;

    /** DOCUMENT ME! */
    private JTextField textPenaltyValue;

    /** DOCUMENT ME! */
    private JTextField textPercentIterationsHalfSize;

    /** DOCUMENT ME! */
    private JTextField textPercentIterationsOriginalSize;

    /** DOCUMENT ME! */
    private JTextField textpercentIterationsQuarterSize;

    /** DOCUMENT ME! */
    private JTextField textTotalNumberIterations;

    /** DOCUMENT ME! */
    private JTextField textWindowLowerLimit;

    /** DOCUMENT ME! */
    private int totalNumberIterations = 100;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private int windowLowerLimit = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogXcosmEM() { }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogXcosmEM(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        originalImage = im;
        userInterface = ViewUserInterface.getReference();
        init();
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
            MipavUtil.showHelp("10086");
        } // end if()-else
    } // end actionPerformed(...)


    /**
     * DOCUMENT ME!
     *
     * @param  algorithm  DOCUMENT ME!
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (((algorithm instanceof AlgorithmXcosmEM) && (xcosmEMAlgo.isCompleted() == true)) && (resultImage != null)) {

            updateFileInfo(originalImage, resultImage);
            resultImage.clearMask();

            try {
                new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            xcosmEMAlgo.finalize();
            xcosmEMAlgo = null;
        }
    } // end algorithmPerformed(...)


    /**
     * Once all the necessary variables are set, call the mean algorithm based on what type of image this is and whether
     * or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name;
        name = makeImageName(originalImage.getImageName(), "_em");

        try {

            if (originalImage.isColorImage()) {
                resultImage = new ModelImage(originalImage.getType(), originalImage.getExtents(), name);
            } else {
                resultImage = new ModelImage(ModelStorageBase.FLOAT, originalImage.getExtents(), name);
            }

            xcosmEMAlgo = new AlgorithmXcosmEM(resultImage, originalImage, psfImage, windowLowerLimit, estimateDecay,
                                               decayConstant, percentIterationsOriginalSize, percentIterationsHalfSize,
                                               percentIterationsQuarterSize, totalNumberIterations,
                                               backupNumberIterations, penaltyIntensity, penaltyValue);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            xcosmEMAlgo.addListener(this);

            createProgressBar(originalImage.getImageName(), xcosmEMAlgo);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (xcosmEMAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                xcosmEMAlgo.run();
            } // end if (isRunInSeparateThread())

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("JDialogXcosmEM: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        } // end try()=catch()

        dispose();
    } // end callAlgorithm()

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(resultImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        originalImage = scriptParameters.retrieveInputImage();
        psfImage = scriptParameters.retrieveImage("psf_image");

        userInterface = ViewUserInterface.getReference();
        parentFrame = originalImage.getParentFrame();

        windowLowerLimit = scriptParameters.getParams().getInt("window_lower_limit");
        estimateDecay = scriptParameters.getParams().getBoolean("do_estimate_decay");
        decayConstant = scriptParameters.getParams().getFloat("decay_constant");
        percentIterationsOriginalSize = scriptParameters.getParams().getInt("percent_iterations_original_size");
        percentIterationsHalfSize = scriptParameters.getParams().getInt("percent_iterations_half_size");
        percentIterationsQuarterSize = scriptParameters.getParams().getInt("percent_iterations_quarter_size");
        totalNumberIterations = scriptParameters.getParams().getInt("total_num_iterations");
        backupNumberIterations = scriptParameters.getParams().getInt("backup_num_iterations");
        penaltyIntensity = scriptParameters.getParams().getBoolean("penalty_intensity");
        penaltyValue = scriptParameters.getParams().getFloat("penalty_value");

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(originalImage);
        scriptParameters.storeImage(psfImage, "psf_image");
        scriptParameters.storeImageInRecorder(resultImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("window_lower_limit", windowLowerLimit));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_estimate_decay", estimateDecay));
        scriptParameters.getParams().put(ParameterFactory.newParameter("decay_constant", decayConstant));
        scriptParameters.getParams().put(ParameterFactory.newParameter("percent_iterations_original_size",
                                                                       percentIterationsOriginalSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("percent_iterations_half_size",
                                                                       percentIterationsHalfSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("percent_iterations_quarter_size",
                                                                       percentIterationsQuarterSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("total_num_iterations", totalNumberIterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("backup_num_iterations",
                                                                       backupNumberIterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("penalty_intensity", penaltyIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("penalty_value", penaltyValue));
    }

    /**
     * Builds a list of images. Returns combobox. List must be all color or all black and white.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    private JComboBox buildComboBox(ModelImage image) {
        ModelImage nextImage;
        boolean doAdd;
        int i;

        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        userInterface = ViewUserInterface.getReference();

        Enumeration names = userInterface.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();

            if (!name.equals(image.getImageName())) {
                nextImage = userInterface.getRegisteredImageByName(name);

                if (userInterface.getFrameContainingImage(nextImage) != null) {

                    if ((image.isColorImage() == nextImage.isColorImage()) &&
                            (image.getNDims() == nextImage.getNDims())) {
                        doAdd = true;

                        for (i = 0; i < image.getNDims(); i++) {

                            if (image.getExtents()[i] != nextImage.getExtents()[i]) {
                                doAdd = false;
                            }
                        }

                        if (doAdd) {
                            comboBox.addItem(name);
                        }
                    }
                }
            }
        }

        return comboBox;
    }

    /**
     * DOCUMENT ME!
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Xcosm Expectation Maximum Restoration");

        JPanel parameterPanel = new JPanel(new GridBagLayout());

        parameterPanel.setForeground(Color.black);
        parameterPanel.setBorder(buildTitledBorder("Parameters"));

        // imageComboBox
        String matchName = originalImage.getImageName();
        labelImage = new JLabel("Restoration of [" + matchName + "] with PSF:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        imageComboBox = buildComboBox(originalImage);
        imageComboBox.addItemListener(this);

        userInterface = ViewUserInterface.getReference();

        String selectedName = (String) imageComboBox.getSelectedItem();

        if (selectedName == null) {
            MipavUtil.displayError("No Point Spread Function Image");

            return;
        }

        psfImage = userInterface.getRegisteredImageByName(selectedName);

        // Window lower limit in Z
        JLabel labelWindowLowerLimit = new JLabel("Window lower limit in Z ");
        labelWindowLowerLimit.setFont(serif12);

        textWindowLowerLimit = new JTextField();
        textWindowLowerLimit.setColumns(5);
        textWindowLowerLimit.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textWindowLowerLimit.setHorizontalAlignment(JTextField.RIGHT);
        textWindowLowerLimit.setText(Integer.toString(windowLowerLimit));
        textWindowLowerLimit.setFont(serif12);

        // Estimate Decay
        radioEstimateDecay = new JRadioButton("Estimate Decay", estimateDecay);
        radioEstimateDecay.setFont(MipavUtil.font12);
        radioEstimateDecay.addActionListener(this);

        // Decay Constant
        JLabel labelDecayConstant = new JLabel("Decay Constant ");
        labelDecayConstant.setFont(serif12);

        textDecayConstant = new JTextField();
        textDecayConstant.setColumns(5);
        textDecayConstant.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textDecayConstant.setHorizontalAlignment(JTextField.RIGHT);
        textDecayConstant.setText(Float.toString(decayConstant));
        textDecayConstant.setFont(serif12);

        // Percent Iterations at Original Size
        JLabel labelPercentIterationsOriginalSize = new JLabel("Original Size Iteration Percentage");
        labelPercentIterationsOriginalSize.setFont(serif12);

        textPercentIterationsOriginalSize = new JTextField();
        textPercentIterationsOriginalSize.setColumns(5);
        textPercentIterationsOriginalSize.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textPercentIterationsOriginalSize.setHorizontalAlignment(JTextField.RIGHT);
        textPercentIterationsOriginalSize.setText(Integer.toString(percentIterationsOriginalSize));
        textPercentIterationsOriginalSize.setFont(serif12);

        // Percent Iterations at oneHalf Size
        JLabel labelPercentIterationsHalfSize = new JLabel("Half Size Iteration Percentage");
        labelPercentIterationsHalfSize.setFont(serif12);

        textPercentIterationsHalfSize = new JTextField();
        textPercentIterationsHalfSize.setColumns(5);
        textPercentIterationsHalfSize.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textPercentIterationsHalfSize.setHorizontalAlignment(JTextField.RIGHT);
        textPercentIterationsHalfSize.setText(Integer.toString(percentIterationsHalfSize));
        textPercentIterationsHalfSize.setFont(serif12);

        // Percent Iterations at Quater Size
        JLabel labelpercentIterationsQuarterSize = new JLabel("Quater Size Iteration Percentage");
        labelpercentIterationsQuarterSize.setFont(serif12);

        textpercentIterationsQuarterSize = new JTextField();
        textpercentIterationsQuarterSize.setColumns(5);
        textpercentIterationsQuarterSize.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textpercentIterationsQuarterSize.setHorizontalAlignment(JTextField.RIGHT);
        textpercentIterationsQuarterSize.setText(Integer.toString(percentIterationsQuarterSize));
        textpercentIterationsQuarterSize.setFont(serif12);


        // Number of Iterations
        JLabel labelTotalNumberIterations = new JLabel("Number of Iterations");
        labelTotalNumberIterations.setFont(serif12);

        textTotalNumberIterations = new JTextField();
        textTotalNumberIterations.setColumns(5);
        textTotalNumberIterations.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textTotalNumberIterations.setHorizontalAlignment(JTextField.RIGHT);
        textTotalNumberIterations.setText(Integer.toString(totalNumberIterations));
        textTotalNumberIterations.setFont(serif12);


        // Backup Number of Iterations
        JLabel labelBackupNumberIterations = new JLabel("Backup Number of Iterations");
        labelBackupNumberIterations.setFont(serif12);

        textBackupNumberIterations = new JTextField();
        textBackupNumberIterations.setColumns(5);
        textBackupNumberIterations.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textBackupNumberIterations.setHorizontalAlignment(JTextField.RIGHT);
        textBackupNumberIterations.setText(Integer.toString(backupNumberIterations));
        textBackupNumberIterations.setFont(serif12);


        GridBagConstraints gbc = new GridBagConstraints();

        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(3, 3, 3, 3);

        gbc.gridy = 0;
        gbc.gridx = 0;
        parameterPanel.add(labelImage, gbc);
        gbc.gridx = 1;
        parameterPanel.add(imageComboBox, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelWindowLowerLimit, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textWindowLowerLimit, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(radioEstimateDecay, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelDecayConstant, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textDecayConstant, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelPercentIterationsOriginalSize, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textPercentIterationsOriginalSize, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelPercentIterationsHalfSize, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textPercentIterationsHalfSize, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelpercentIterationsQuarterSize, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textpercentIterationsQuarterSize, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelTotalNumberIterations, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textTotalNumberIterations, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelBackupNumberIterations, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textBackupNumberIterations, gbc);


        ButtonGroup penaltyGroup = new ButtonGroup();

        radioIntensity = new JRadioButton("Intensity Penalty", penaltyIntensity);
        radioIntensity.setFont(MipavUtil.font12);

        // radioIntensity.setActionCommand("EntireImage");
        radioIntensity.addActionListener(this);
        penaltyGroup.add(radioIntensity);

        radioRoughness = new JRadioButton("Roughness Penalty", !penaltyIntensity);
        radioRoughness.setFont(MipavUtil.font12);

        // radioRoughness.setActionCommand("VOIRegion");
        radioRoughness.addActionListener(this);
        penaltyGroup.add(radioRoughness);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(radioIntensity, gbc);
        gbc.gridx = 1;
        parameterPanel.add(radioRoughness, gbc);


        // Penalty Value
        JLabel labelPenaltyValue = new JLabel("Penalty Value");
        labelPenaltyValue.setFont(serif12);

        textPenaltyValue = new JTextField();
        textPenaltyValue.setColumns(5);
        textPenaltyValue.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textPenaltyValue.setHorizontalAlignment(JTextField.RIGHT);
        textPenaltyValue.setText(Float.toString(penaltyValue));
        textPenaltyValue.setFont(serif12);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelPenaltyValue, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textPenaltyValue, gbc);


        ButtonGroup regionGroup = new ButtonGroup();

        radioEntireImage = new JRadioButton("Entire image", entireImage);
        radioEntireImage.setFont(MipavUtil.font12);
        radioEntireImage.setActionCommand("EntireImage");
        radioEntireImage.addActionListener(this);
        regionGroup.add(radioEntireImage);

        radioVOIRegion = new JRadioButton("VOI regions", !entireImage);
        radioVOIRegion.setFont(MipavUtil.font12);
        radioVOIRegion.setActionCommand("VOIRegion");
        radioVOIRegion.addActionListener(this);
        regionGroup.add(radioVOIRegion);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(radioEntireImage, gbc);
        gbc.gridx = 1;
        parameterPanel.add(radioVOIRegion, gbc);

        gbc.gridx = 0;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel mainPanel = new JPanel(new GridBagLayout());

        gbc.gridy = 0;
        mainPanel.add(parameterPanel, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        //setResizable(false);

    } // end init()


    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {
        String tmpStr;

        userInterface = ViewUserInterface.getReference();

        String selectedName = (String) imageComboBox.getSelectedItem();
        psfImage = userInterface.getRegisteredImageByName(selectedName);

        if (psfImage == null) {
            return false;
        }

        tmpStr = textWindowLowerLimit.getText();
        windowLowerLimit = Integer.parseInt(tmpStr);

        estimateDecay = radioEstimateDecay.isSelected();

        tmpStr = textDecayConstant.getText();
        decayConstant = Float.parseFloat(tmpStr);

        tmpStr = textPercentIterationsOriginalSize.getText();
        percentIterationsOriginalSize = Integer.parseInt(tmpStr);

        tmpStr = textPercentIterationsHalfSize.getText();
        percentIterationsHalfSize = Integer.parseInt(tmpStr);

        tmpStr = textpercentIterationsQuarterSize.getText();
        percentIterationsQuarterSize = Integer.parseInt(tmpStr);

        tmpStr = textTotalNumberIterations.getText();
        totalNumberIterations = Integer.parseInt(tmpStr);

        tmpStr = textBackupNumberIterations.getText();
        backupNumberIterations = Integer.parseInt(tmpStr);

        penaltyIntensity = radioIntensity.isSelected();

        tmpStr = textPenaltyValue.getText();
        penaltyValue = Float.parseFloat(tmpStr);

        entireImage = radioEntireImage.isSelected();

        return true;
    } // end setVariables()


} // end JDialogXcosmEM
