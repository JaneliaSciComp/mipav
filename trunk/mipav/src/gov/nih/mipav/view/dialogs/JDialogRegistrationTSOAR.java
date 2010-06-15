package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegTSOAR;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.Parameter;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterFloat;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterList;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.ActionMetadata.ImageRequirements;

import java.awt.*;
import java.awt.event.*;
import java.util.EnumSet;
import java.util.Set;

import javax.swing.*;


/**
 * Dialog to get options for running the time series registration. Cost function, interpolation, mean or center volume,
 * etc.
 * 
 * @author Neva Cherniavsky
 * @see gov.nih.mipav.model.algorithms.registration#AlgorithmRegistrationTSOAR
 */
public class JDialogRegistrationTSOAR extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5174224884360217569L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmRegTSOAR algoReg;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;

    /** DOCUMENT ME! */
    private JComboBox comboBoxDOF;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp2;

    /** DOCUMENT ME! */
    private int cost, interp, interp2, DOF;

    /** DOCUMENT ME! */
    private boolean displayTransform, useReferenceTimeSlice;

    /** DOCUMENT ME! */
    private boolean doGraph;

    /** DOCUMENT ME! */
    private boolean doSubsample;

    /** DOCUMENT ME! */
    private boolean finalRegistrationAtLevel2;

    /** DOCUMENT ME! */
    private JCheckBox graphCheckBox;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JLabel labelInterp2;

    /** DOCUMENT ME! */
    private JRadioButton meanButton;

    /** DOCUMENT ME! */
    private JRadioButton referenceButton;

    /** DOCUMENT ME! */
    private int refImageNum = 0;

    /** DOCUMENT ME! */
    private JTextField refImageNumText;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private JCheckBox sampleCheckBox;

    /** DOCUMENT ME! */
    private JCheckBox sampleCheckBoxLevel2;

    /** DOCUMENT ME! */
    private JCheckBox sincCheckBox;

    /** DOCUMENT ME! */
    private boolean sincNormalizedCrossCorrelation;

    /** DOCUMENT ME! */
    private JCheckBox transformCheckbox;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegistrationTSOAR() {}

    /**
     * Creates new dialog with given parent frame and image to register.
     * 
     * @param parent Parent frame.
     * @param img Image to register.
     */
    public JDialogRegistrationTSOAR(final Frame parent, final ModelImage img) {
        super(parent, true);
        image = img;
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Sets up the variables and calls the algorithm.
     * 
     * @param evt Event that triggered this function.
     */
    public void actionPerformed(final ActionEvent evt) {
        final String command = evt.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }

            dispose();
        } else if (command.equals("Cancel")) {
            dispose();
        }
    }

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        float[][] rot = null;
        float[][] posR = null;
        float[][] trans = null;
        float[][] posT = null;
        int i;

        if (algorithm instanceof AlgorithmRegTSOAR) {

            if (algoReg.isCompleted()) {

                if (displayTransform) {
                    final String name = JDialogBase.makeImageName(image.getImageName(), "_register");

                    resultImage = algoReg.getTransformedImage(interp2);
                    resultImage.calcMinMax();
                    resultImage.setImageName(name);

                    if (resultImage != null) {

                        try {
                            new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                        } catch (final OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }
                }

                if (doGraph) {
                    rot = algoReg.getRot();
                    posR = new float[3][image.getExtents()[3]];

                    for (i = 0; i < image.getExtents()[3]; i++) {
                        posR[0][i] = i + 1.0f;
                        posR[1][i] = i + 1.0f;
                        posR[2][i] = i + 1.0f;
                    }

                    final ViewJFrameGraph rotGraph = new ViewJFrameGraph(posR, rot, "Rotations", "Volume number",
                            "Degrees");
                    rotGraph.makeRangeSymmetric();
                    rotGraph.showXYZLegends();
                    rotGraph.setDefaultDirectory(ViewUserInterface.getReference().getDefaultDirectory());
                    rotGraph.setVisible(true);
                    trans = algoReg.getTrans();
                    posT = new float[3][image.getExtents()[3]];

                    for (i = 0; i < image.getExtents()[3]; i++) {
                        posT[0][i] = i + 1.0f;
                        posT[1][i] = i + 1.0f;
                        posT[2][i] = i + 1.0f;
                    }

                    final ViewJFrameGraph transGraph = new ViewJFrameGraph(posT, trans, "Translations",
                            "Volume number", "Translations in "
                                    + FileInfoBase
                                            .getUnitsOfMeasureAbbrevStr(image.getFileInfo(0).getUnitsOfMeasure(0)));
                    transGraph.makeRangeSymmetric();
                    transGraph.showXYZLegends();
                    transGraph.setDefaultDirectory(ViewUserInterface.getReference().getDefaultDirectory());
                    transGraph.setVisible(true);
                } // if (doGraph)
            }

            if (algoReg != null) {
                algoReg.disposeLocal();
                algoReg = null;
            }

            if (rot != null) {

                for (i = 0; i < rot.length; i++) {
                    rot[i] = null;
                }

                rot = null;
            }

            if (posR != null) {

                for (i = 0; i < posR.length; i++) {
                    posR[i] = null;
                }

                posR = null;
            }

            if (trans != null) {

                for (i = 0; i < trans.length; i++) {
                    trans[i] = null;
                }

                trans = null;
            }

            if (posT != null) {

                for (i = 0; i < posT.length; i++) {
                    posT[i] = null;
                }

                posT = null;
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }
    }

    /**
     * Accessor to get the result image.
     * 
     * @return Result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Changes the interpolation box to enabled or disabled depending on if the transform box is checked or not.
     * 
     * @param event Event that triggered this function.
     */
    public void itemStateChanged(final ItemEvent event) {

        if (event.getSource() == transformCheckbox) {
            comboBoxInterp2.setEnabled(transformCheckbox.isSelected());
            labelInterp2.setEnabled(transformCheckbox.isSelected());
        } else if (event.getSource() == comboBoxCostFunct) {

            if (comboBoxCostFunct.getSelectedIndex() == 2) {
                sincCheckBox.setEnabled(true);
            } else {
                sincCheckBox.setEnabled(false);
                sincCheckBox.setSelected(false);
            }
        } else if (event.getSource() == comboBoxDOF) {

            if (comboBoxDOF.getSelectedIndex() == 0) {
                graphCheckBox.setEnabled(true);
            } else {
                graphCheckBox.setEnabled(false);
                graphCheckBox.setSelected(false);
            }
        } else if ( (event.getSource() == meanButton) || (event.getSource() == referenceButton)) {
            if (referenceButton.isSelected()) {
                refImageNumText.setEnabled(true);
            } else {
                refImageNumText.setEnabled(false);
            }
        }
    }

    /**
     * Accessor to set the choice of cost function.
     * 
     * @param x Cost function.
     */
    public void setCostChoice(final int x) {
        cost = x;
    }

    /**
     * Accessor to set whether or not the graph of the output should occur.
     * 
     * @param doDisplay if true graph result of translations and rotations
     */
    public void setDisplayResult(final boolean doDisplay) {
        displayTransform = doDisplay;
    }

    /**
     * Accessor to set the degrees of freedom.
     * 
     * @param x Degrees of freedom
     */
    public void setDOF(final int x) {
        DOF = x;
    }

    /**
     * Accessor to set whether or not subsampling occurs.
     * 
     * @param doFinalXCorrSinc if true subsample the image
     */
    public void setFinalCostXCorrSinc(final boolean doFinalXCorrSinc) {
        sincNormalizedCrossCorrelation = doFinalXCorrSinc;
    }

    /**
     * Accessor to set whether or not subsampling occurs.
     * 
     * @param setFinalRegAtLevel2 DOCUMENT ME!
     */
    public void setFinalRegistrationAtLevel2(final boolean setFinalRegAtLevel2) {
        this.finalRegistrationAtLevel2 = setFinalRegAtLevel2;
    }

    /**
     * Accessor to set whether or not the graph of the output should occur.
     * 
     * @param doGraph if true graph result of translations and rotations
     */
    public void setGraph(final boolean doGraph) {
        this.doGraph = doGraph;
    }

    /**
     * Accessor to set graphCheckBox.
     * 
     * @param doGraph if true output graphs of rotations and translations
     */
    public void setGraphCheckBox(final boolean doGraph) {
        this.doGraph = doGraph;
    }

    /**
     * Accessor to set the initial interpolation.
     * 
     * @param x Interpolation
     */
    public void setInterp(final int x) {
        interp = x;
    }

    /**
     * Accessor to set the final interpolation.
     * 
     * @param x Interpolation
     */
    public void setInterp2(final int x) {
        interp2 = x;
    }

    /**
     * Accessor to set refImageNum.
     * 
     * @param refImageNumber number of reference volume
     */
    public void setRefImageNum(final int refImageNumber) {
        refImageNum = refImageNumber;
    }

    /**
     * Accessor to set whether or not subsampling occurs.
     * 
     * @param doSubsample if true subsample the image
     */
    public void setSubsample(final boolean doSubsample) {
        this.doSubsample = doSubsample;
    }

    /**
     * Accessor to set whether or not to use average volume as a referece.
     * 
     * @param useAverageVolumeRef if true use average volume as the referenc image
     */
    public void setUseAverageVolumeRef(final boolean useAverageVolumeRef) {
        this.useReferenceTimeSlice = !useAverageVolumeRef;
    }

    /**
     * Calls the time series registration algorithm.
     */
    protected void callAlgorithm() {
        algoReg = new AlgorithmRegTSOAR(image, cost, DOF, interp, useReferenceTimeSlice, refImageNum,
                sincNormalizedCrossCorrelation, doGraph, doSubsample, finalRegistrationAtLevel2);
        algoReg.addListener(this);

        createProgressBar(image.getImageName(), algoReg);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (algoReg.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            algoReg.run();
        }

        dispose();
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (getResultImage() != null) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        setCostChoice(scriptParameters.getParams().getInt("cost_function_type"));
        setDOF(scriptParameters.getParams().getInt("degrees_of_freedom"));
        setInterp(scriptParameters.getParams().getInt("initial_interpolation_type"));
        setUseAverageVolumeRef(scriptParameters.getParams().getBoolean("do_use_avg_volume_as_reference"));
        setRefImageNum(scriptParameters.getParams().getInt("reference_volume_num"));
        setFinalCostXCorrSinc(scriptParameters.getParams().getBoolean("do_use_norm_cross_correlation_sinc"));
        setGraph(scriptParameters.getParams().getBoolean("do_graph_transform"));
        setSubsample(scriptParameters.getParams().getBoolean("do_subsample"));
        setFinalRegistrationAtLevel2(scriptParameters.getParams().getBoolean("do_final_reg_at_level_2"));
        setDisplayResult(scriptParameters.getParams().getBoolean("do_display_transform"));
        setInterp2(scriptParameters.getParams().getInt("final_interpolation_type"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (getResultImage() != null));

        scriptParameters.getParams().put(ParameterFactory.newParameter("cost_function_type", cost));
        scriptParameters.getParams().put(ParameterFactory.newParameter("degrees_of_freedom", DOF));
        scriptParameters.getParams().put(ParameterFactory.newParameter("initial_interpolation_type", interp));
        scriptParameters.getParams().put(ParameterFactory.newParameter("final_interpolation_type", interp2));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter("do_use_avg_volume_as_reference", !useReferenceTimeSlice));
        scriptParameters.getParams().put(ParameterFactory.newParameter("reference_volume_num", refImageNum));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter("do_use_norm_cross_correlation_sinc", sincNormalizedCrossCorrelation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_graph_transform", doGraph));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_subsample", doSubsample));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter("do_final_reg_at_level_2", finalRegistrationAtLevel2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_display_transform", displayTransform));
    }

    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        setTitle("Time Series Registration");

        final JPanel panel = new JPanel(new GridBagLayout());
        panel.setBorder(buildTitledBorder("Input Options"));

        final JLabel labelDOF = new JLabel("Degrees of freedom:");
        labelDOF.setForeground(Color.black);
        labelDOF.setFont(serif12);
        labelDOF.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxDOF = new JComboBox();
        comboBoxDOF.setFont(MipavUtil.font12);
        comboBoxDOF.setBackground(Color.white);
        comboBoxDOF.setToolTipText("Degrees of freedom");
        comboBoxDOF.addItem("Rigid - 6");
        comboBoxDOF.addItem("Global rescale - 7");
        comboBoxDOF.addItem("Specific rescale - 9");
        comboBoxDOF.addItem("Affine - 12");
        comboBoxDOF.setSelectedIndex(0);
        comboBoxDOF.addItemListener(this);

        final JLabel labelCost = new JLabel("Cost function:");
        labelCost.setForeground(Color.black);
        labelCost.setFont(serif12);
        labelCost.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxCostFunct = new JComboBox();
        comboBoxCostFunct.setFont(MipavUtil.font12);
        comboBoxCostFunct.setBackground(Color.white);
        comboBoxCostFunct.setToolTipText("Cost function");
        if (image.isColorImage()) {
            comboBoxCostFunct.addItem("Least squares");
            comboBoxCostFunct.setSelectedIndex(0);
        } else {
            comboBoxCostFunct.addItem("Correlation ratio");
            comboBoxCostFunct.addItem("Least squares");
            comboBoxCostFunct.addItem("Normalized cross correlation");
            comboBoxCostFunct.addItem("Normalized mutual information");
            comboBoxCostFunct.setSelectedIndex(2);
        }
        comboBoxCostFunct.addItemListener(this);

        final JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp.addItem("Trilinear");
        comboBoxInterp.addItem("Bspline 3rd order");
        comboBoxInterp.addItem("Bspline 4th order");
        comboBoxInterp.addItem("Cubic Lagrangian");
        comboBoxInterp.addItem("Quintic Lagrangian");
        comboBoxInterp.addItem("Heptic Lagrangian");
        comboBoxInterp.addItem("Windowed sinc");
        // comboBoxInterp.addItem("Nearest Neighbor");

        sincCheckBox = new JCheckBox("Finalize with normalized cross correlation sinc");
        sincCheckBox.setFont(serif12);
        sincCheckBox.setForeground(Color.black);
        sincCheckBox.setSelected(false);
        if (image.isColorImage()) {
            sincCheckBox.setEnabled(false);
        }

        sampleCheckBox = new JCheckBox("Subsample image for speed");
        sampleCheckBox.setFont(serif12);
        sampleCheckBox.setForeground(Color.black);
        sampleCheckBox.setSelected(true);
        sampleCheckBox.setEnabled(true);

        sampleCheckBoxLevel2 = new JCheckBox("Set final subsample at Level 2 (speeds registration)");
        sampleCheckBoxLevel2.setFont(serif12);
        sampleCheckBoxLevel2.setForeground(Color.black);
        sampleCheckBoxLevel2.setSelected(false);
        sampleCheckBoxLevel2.setEnabled(true);

        final ButtonGroup group = new ButtonGroup();

        referenceButton = new JRadioButton("Register to reference volume(0-"
                + String.valueOf(image.getExtents()[3] - 1) + ")");
        referenceButton.setSelected(true);
        referenceButton.setFont(serif12);
        referenceButton.setForeground(Color.black);
        referenceButton.addItemListener(this);
        group.add(referenceButton);

        refImageNumText = new JTextField("0", 3);
        refImageNumText.setEnabled(true);

        meanButton = new JRadioButton("Register to average volume");
        meanButton.setFont(serif12);
        meanButton.setForeground(Color.black);
        meanButton.addItemListener(this);
        group.add(meanButton);

        final Insets insets = new Insets(0, 2, 0, 2);
        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = insets;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        panel.add(labelDOF, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        panel.add(comboBoxDOF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        panel.add(labelInterp, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        panel.add(comboBoxInterp, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        panel.add(labelCost, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        panel.add(comboBoxCostFunct, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        panel.add(sincCheckBox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        panel.add(sampleCheckBox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        panel.add(sampleCheckBoxLevel2, gbc);

        gbc.gridx = 0;
        gbc.gridy = 6;
        panel.add(referenceButton, gbc);

        gbc.gridx = 1;
        gbc.gridy = 6;
        panel.add(refImageNumText, gbc);

        gbc.gridx = 0;
        gbc.gridy = 7;
        panel.add(meanButton, gbc);

        final JPanel outPanel = new JPanel();
        outPanel.setLayout(new GridBagLayout());
        outPanel.setBorder(buildTitledBorder("Output Options"));

        transformCheckbox = new JCheckBox("Display transformed image");
        transformCheckbox.setFont(serif12);
        transformCheckbox.setForeground(Color.black);
        transformCheckbox.setSelected(true);
        transformCheckbox.addItemListener(this);

        labelInterp2 = new JLabel("Interpolation:");
        labelInterp2.setForeground(Color.black);
        labelInterp2.setFont(serif12);
        labelInterp2.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp2 = new JComboBox();
        comboBoxInterp2.setFont(serif12);
        comboBoxInterp2.setBackground(Color.white);
        comboBoxInterp2.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp2.addItem("Trilinear");
        comboBoxInterp2.addItem("Bspline 3rd order");
        comboBoxInterp2.addItem("Bspline 4th order");
        comboBoxInterp2.addItem("Cubic Lagrangian");
        comboBoxInterp2.addItem("Quintic Lagrangian");
        comboBoxInterp2.addItem("Heptic Lagrangian");
        comboBoxInterp2.addItem("Windowed sinc");
        comboBoxInterp2.addItem("Nearest Neighbor");

        graphCheckBox = new JCheckBox("Graph rotations and translations");
        graphCheckBox.setFont(serif12);
        graphCheckBox.setForeground(Color.black);
        graphCheckBox.setSelected(false);
        graphCheckBox.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(transformCheckbox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        outPanel.add(labelInterp2, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(comboBoxInterp2, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(graphCheckBox, gbc);

        final JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(panel);
        mainPanel.add(outPanel, BorderLayout.SOUTH);

        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Sets the variables based on what was selected in the GUI. Returns flag indicating success.
     * 
     * @return <code>true</code> if successful.
     */
    private boolean setVariables() {

        switch (comboBoxDOF.getSelectedIndex()) {

            case 0:
                DOF = 6;
                break;

            case 1:
                DOF = 7;
                break;

            case 2:
                DOF = 9;
                break;

            case 3:
                DOF = 12;
                break;

            default:
                DOF = 12;
                break;
        }

        switch (comboBoxInterp.getSelectedIndex()) {

            case 0:
                interp = AlgorithmTransform.TRILINEAR;
                break;

            case 1:
                interp = AlgorithmTransform.BSPLINE3;
                break;

            case 2:
                interp = AlgorithmTransform.BSPLINE4;
                break;

            case 3:
                interp = AlgorithmTransform.CUBIC_LAGRANGIAN;
                break;

            case 4:
                interp = AlgorithmTransform.QUINTIC_LAGRANGIAN;
                break;

            case 5:
                interp = AlgorithmTransform.HEPTIC_LAGRANGIAN;
                break;

            case 6:
                interp = AlgorithmTransform.WSINC;
                break;
            // case 7: interp = AlgorithmTransform.NEAREST_NEIGHBOR; break;

            default:
                interp = AlgorithmTransform.TRILINEAR;
                break;
        }

        if (image.isColorImage()) {
            switch (comboBoxCostFunct.getSelectedIndex()) {

                case 0:
                    cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_COLOR;
                    break;
                default:
                    cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_COLOR;
            }
        } else { // black and white image
            switch (comboBoxCostFunct.getSelectedIndex()) {

                case 0:
                    cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                    break;

                case 1:
                    cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED;
                    break;

                case 2:
                    cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION;
                    break;

                case 3:
                    cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
                    break;

                default:
                    cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION;
                    break;
            }
        }

        sincNormalizedCrossCorrelation = sincCheckBox.isSelected();

        switch (comboBoxInterp2.getSelectedIndex()) {

            case 0:
                interp2 = AlgorithmTransform.TRILINEAR;
                break;

            case 1:
                interp2 = AlgorithmTransform.BSPLINE3;
                break;

            case 2:
                interp2 = AlgorithmTransform.BSPLINE4;
                break;

            case 3:
                interp2 = AlgorithmTransform.CUBIC_LAGRANGIAN;
                break;

            case 4:
                interp2 = AlgorithmTransform.QUINTIC_LAGRANGIAN;
                break;

            case 5:
                interp2 = AlgorithmTransform.HEPTIC_LAGRANGIAN;
                break;

            case 6:
                interp2 = AlgorithmTransform.WSINC;
                break;

            case 7:
                interp2 = AlgorithmTransform.NEAREST_NEIGHBOR;
                break;

            default:
                interp2 = AlgorithmTransform.TRILINEAR;
                break;
        }

        displayTransform = transformCheckbox.isSelected();
        useReferenceTimeSlice = referenceButton.isSelected();

        if (useReferenceTimeSlice) {
            if ( !JDialogBase.testParameter(refImageNumText.getText(), 0, image.getExtents()[3] - 1)) {
                refImageNumText.requestFocus();
                refImageNumText.selectAll();

                return false;
            } else {
                refImageNum = Integer.valueOf(refImageNumText.getText()).intValue();
            }
        } // if (reference)

        doGraph = graphCheckBox.isSelected();
        doSubsample = sampleCheckBox.isSelected();
        finalRegistrationAtLevel2 = sampleCheckBoxLevel2.isSelected();

        return true;
    }
    
    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Registration");
            }

            public String getDescription() {
                return new String("Performs a time series registration on a 4D volume." +
                		"Note: For degrees_of_freedom, O == 6 (Rigid), 1 == 7 (Global Rescale)," +
                		"2 == 9 (Specific Rescale), and 3 == 12 (Affine)." +
                		"For initial_interpolation_type and final_interpolation_type, 0 == TRILINEAR, 1 == BSPLINE3, " +
                		"2 == BSPLINE4, 3 == CUBIC_LAGRANGIAN, 4 == QUINTIC_LAGRANGIAN, " +
                		"5 == HEPTIC_LAGRANGIAN, and 6 == WSINC." +
                		"For cost_function_type, 0 == CORRELATION_RATIO_SMOOTHED, 1 == LEAST_SQUARES_SMOOTHED" +
                		"2 == NORMALIZED_XCORRELATION, and 3 == NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, " +
                		"1,2, and 3 being only applicable to black and white images.");
            }

            public String getDescriptionLong() {
                return new String("Performs a time series registration on a 4D volume." +
                		"Note: For degrees_of_freedom, O == 6 (Rigid), 1 == 7 (Global Rescale)," +
                		"2 == 9 (Specific Rescale), and 3 == 12 (Affine)." +
                		"For initial_interpolation_type and final_interpolation_type, 0 == TRILINEAR, 1 == BSPLINE3, " +
                		"2 == BSPLINE4, 3 == CUBIC_LAGRANGIAN, 4 == QUINTIC_LAGRANGIAN, " +
                		"5 == HEPTIC_LAGRANGIAN, and 6 == WSINC." +
                		"For cost_function_type, 0 == CORRELATION_RATIO_SMOOTHED, 1 == LEAST_SQUARES_SMOOTHED" +
                		"2 == NORMALIZED_XCORRELATION, and 3 == NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, " +
                		"1,2, and 3 being only applicable to black and white images.");
            }

            public String getShortLabel() {
                return new String("TSOAR");
            }

            public String getLabel() {
                return new String("Time Series Registration");
            }

            public String getName() {
                return new String("Time Series Registration");
            }

            public Set<ImageRequirements> getInputImageRequirements() {
                return EnumSet.of(ImageRequirements.NDIM_4);
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));

            //Input options
            
            //O == 6 (Rigid)
            //1 == 7 (Global Rescale)
            //2 == 9 (Specific Rescale)
            //3 == 12 (Affine)
            table.put(new ParameterInt("degrees_of_freedom", 6));
            
            //0 == TRILINEAR
            //1 == BSPLINE3
            //2 == BSPLINE4
            //3 == CUBIC_LAGRANGIAN
            //4 == QUINTIC_LAGRANGIAN
            //5 == HEPTIC_LAGRANGIAN
            //6 == WSINC
            table.put(new ParameterInt("initial_interpolation_type", 0));
            
            //0 == CORRELATION_RATIO_SMOOTHED
            	//1, 2, 3 only for black and white images
            //1 == LEAST_SQUARES_SMOOTHED
            //2 == NORMALIZED_XCORRELATION
            //3 == NORMALIZED_MUTUAL_INFORMATION_SMOOTHED
            table.put(new ParameterInt("cost_function_type", 2));
            
            table.put(new ParameterBoolean("do_use_norm_cross_correlation_sinc", false));
            table.put(new ParameterBoolean("do_subsample", true));
            table.put(new ParameterBoolean("do_final_reg_at_level_2", false));
            table.put(new ParameterInt("reference_volume_num", 0));        
            //If false, register to a reference volume as set by reference_volume_num
            table.put(new ParameterBoolean("do_use_avg_volume_as_reference", false));

            table.put(new ParameterBoolean("do_display_transform", true));
            table.put(new ParameterBoolean("do_use_max_of_min_resolutions", true));
            table.put(new ParameterBoolean("do_subsample", true));
            table.put(new ParameterBoolean("do_use_fast_mode", true));
            table.put(new ParameterBoolean("do_calc_COG", true));
            
            //Output options
            table.put(new ParameterBoolean("do_display_transform", true));
            table.put(new ParameterInt("final_interpolation_type", 0));
            table.put(new ParameterBoolean("do_graph_transform", false));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }
}
