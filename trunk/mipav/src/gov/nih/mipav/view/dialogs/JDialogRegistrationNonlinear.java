package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.text.*;

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogRegistrationNonlinear extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2978757842164577289L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int addInternal = 0;

    /** DOCUMENT ME! */
    private JRadioButton adjacentButton;

    /** DOCUMENT ME! */
    private JCheckBox clipCheckBox;

    /** DOCUMENT ME! */
    private boolean clipOutput = true;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;

    /** DOCUMENT ME! */
    private JComboBox comboBoxDOF, comboBoxInterp;

    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private int costFxn; // 1 = scaled least squares, 2 = correlation ratio

    /** DOCUMENT ME! */
    private boolean doAdjacent = true;

    /** DOCUMENT ME! */
    private boolean doInternal = false;

    /** DOCUMENT ME! */
    private boolean entireSource = true;

    /** DOCUMENT ME! */
    private boolean entireTarget = true;

    /** DOCUMENT ME! */
    private double[][] esInitial = null;

    /** DOCUMENT ME! */
    private boolean interaction;

    /** DOCUMENT ME! */
    private JCheckBox interactionCheckBox;

    /** DOCUMENT ME! */
    private ButtonGroup internalGroup;

    /** DOCUMENT ME! */
    private int interpolation;

    /** DOCUMENT ME! */
    private int iterations;

    /** DOCUMENT ME! */
    private JLabel labelInternal;

    /** DOCUMENT ME! */
    private JLabel labelSourceGaussZ;

    /** DOCUMENT ME! */
    private JLabel labelTargetGaussX;

    /** DOCUMENT ME! */
    private JLabel labelTargetGaussY;

    /** DOCUMENT ME! */
    private JLabel labelTargetGaussZ;

    /** DOCUMENT ME! */
    private JLabel labelTargetThreshold;

    /** DOCUMENT ME! */
    private ModelImage matchImage; // register source image to refImage

    /** DOCUMENT ME! */
    private DecimalFormat nf;

    /** DOCUMENT ME! */
    private AlgorithmRegNonlinear nl = null;

    /** DOCUMENT ME! */
    private JCheckBox parameterCheckBox;

    /** DOCUMENT ME! */
    private float precision;

    /** DOCUMENT ME! */
    private JRadioButton referenceButton;

    /** DOCUMENT ME! */
    private int referenceSlice;

    /** DOCUMENT ME! */
    private ModelImage refImage;

    /** DOCUMENT ME! */
    private int[] resultExtents;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private int sampleFactor2;

    /** DOCUMENT ME! */
    private String selectedName;

    /** DOCUMENT ME! */
    private float sourceGaussX = 0.0f;

    /** DOCUMENT ME! */
    private float sourceGaussY = 0.0f;

    /** DOCUMENT ME! */
    private float sourceGaussZ = 0.0f;

    /** DOCUMENT ME! */
    private String sourceName;

    /** DOCUMENT ME! */
    private float sourceThreshold;

    /** DOCUMENT ME! */
    private JCheckBox sourceVOICheckBox;

    /** DOCUMENT ME! */
    private float targetGaussX = 0.0f;

    /** DOCUMENT ME! */
    private float targetGaussY = 0.0f;

    /** DOCUMENT ME! */
    private float targetGaussZ = 0.0f;

    /** DOCUMENT ME! */
    private float targetThreshold;

    /** DOCUMENT ME! */
    private JCheckBox targetVOICheckBox;

    /** DOCUMENT ME! */
    private int targetXDim, targetYDim, targetZDim;

    /** DOCUMENT ME! */
    private JTextField textFinalSampling;

    /** DOCUMENT ME! */
    private JTextField textInternal;

    /** DOCUMENT ME! */
    private JTextField textIterations;

    /** DOCUMENT ME! */
    private JTextField textPrecision;

    /** DOCUMENT ME! */
    private JTextField textSourceGaussX;

    /** DOCUMENT ME! */
    private JTextField textSourceGaussY;

    /** DOCUMENT ME! */
    private JTextField textSourceGaussZ;

    /** DOCUMENT ME! */
    private JTextField textSourceThreshold;

    /** DOCUMENT ME! */
    private JTextField textTargetGaussX;

    /** DOCUMENT ME! */
    private JTextField textTargetGaussY;

    /** DOCUMENT ME! */
    private JTextField textTargetGaussZ;

    /** DOCUMENT ME! */
    private JTextField textTargetThreshold;

    /** DOCUMENT ME! */
    private String tmpStr;

    /** DOCUMENT ME! */
    private int transformation;

    /** DOCUMENT ME! */
    private boolean transformVOI = false;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private JCheckBox voiCheckbox;

    /** DOCUMENT ME! */
    private boolean writeParameters = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegistrationNonlinear() { }

    /**
     * Creates new registration dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogRegistrationNonlinear(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);
        matchImage = im;
        UI = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        nf = new DecimalFormat("####0.0####");
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  _UI  The user interface, needed to create the image frame.
     * @param  im   Source image.
     */
    public JDialogRegistrationNonlinear(ViewUserInterface _UI, ModelImage im) {
        super();
        UI = _UI;
        matchImage = im;
        parentFrame = im.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets up the variables needed for running the algorithm, and
     * calls the algorithm.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == OKButton) {

            if (setVariables()) {
                setVisible(false);
                callAlgorithm();
            }
        } else if (source == cancelButton) {
            dispose();
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmRegNonlinear) {
            matchImage.clearMask();

            if ((nl.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(matchImage, resultImage);
                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
                System.gc();
            }
        }

        // Update frame
        matchImage.notifyImageDisplayListeners(null, true);

        insertScriptLine(algorithm);

        if (nl != null) {
            nl.finalize();
            nl = null;
        }

        dispose();
    }

    /**
     * Accessor to get the result image.
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

            if (UI.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (UI.getScriptDialog().getImgTableVar(matchImage.getImageName()) == null) {

                    if (UI.getScriptDialog().getActiveImgTableVar(matchImage.getImageName()) == null) {
                        UI.getScriptDialog().putActiveVar(matchImage.getImageName());
                    }
                }

                // check to see if the ref image is already in the ImgTable
                if (UI.getScriptDialog().getImgTableVar(refImage.getImageName()) == null) {

                    if (UI.getScriptDialog().getActiveImgTableVar(refImage.getImageName()) == null) {
                        UI.getScriptDialog().putActiveVar(refImage.getImageName());
                    }
                }

                UI.getScriptDialog().putVar(resultImage.getImageName());
                UI.getScriptDialog().append("RegistrationNonlinear ");
                UI.getScriptDialog().append(UI.getScriptDialog().getVar(matchImage.getImageName()) + " " +
                                            UI.getScriptDialog().getVar(refImage.getImageName()) + " " +
                                            UI.getScriptDialog().getVar(resultImage.getImageName()) + " " +
                                            entireTarget + " " + entireSource + " " + transformation + " " +
                                            interpolation + " " + clipOutput + " " + interaction + " " + transformVOI +
                                            " " + sourceThreshold + " " + targetThreshold + " " + sourceGaussX + " " +
                                            sourceGaussY + " " + sourceGaussZ + " " + targetGaussX + " " +
                                            targetGaussY + " " + targetGaussZ + " " + sampleFactor2 + " " + costFxn +
                                            " " + precision + " " + iterations + " " + writeParameters + "\n");
            }
        }
    }

    /**
     * Method to handle item events. If image name was changed in the combo box, reset labels for target threshold.
     *
     * @param  event  Event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        String selectedInterp;
        Object source = event.getSource();

        if (source == comboBoxImage) {
            selectedName = (String) comboBoxImage.getSelectedItem();
            refImage = UI.getRegisteredImageByName(selectedName);
            targetThreshold = (float) (refImage.getMin() + (0.2f * (refImage.getMax() - refImage.getMin())));

            if (labelTargetThreshold != null) {
                labelTargetThreshold.setText("Target threshold (" + nf.format(refImage.getMin()) + " - " +
                                             nf.format(refImage.getMax()) + ")");
            }

            if (textTargetThreshold != null) {

                if (doInternal) {
                    textTargetThreshold.setText(" ");
                } else {
                    textTargetThreshold.setText(String.valueOf(targetThreshold));
                }
            }

            if ((sourceName == selectedName) && (matchImage.getNDims() == 3)) {
                doInternal = true;
                labelInternal.setEnabled(true);
                textInternal.setEnabled(true);
                adjacentButton.setEnabled(true);
                referenceButton.setEnabled(true);
                labelSourceGaussZ.setEnabled(false);
                textSourceGaussZ.setEnabled(false);
                labelTargetGaussX.setEnabled(false);
                textTargetGaussX.setEnabled(false);
                textTargetGaussX.setText(" ");
                labelTargetGaussY.setEnabled(false);
                textTargetGaussY.setEnabled(false);
                textTargetGaussY.setText(" ");
                labelTargetGaussZ.setEnabled(false);
                textTargetGaussZ.setEnabled(false);
                textTargetGaussZ.setText(" ");
                targetVOICheckBox.setEnabled(false);

                if ((matchImage.getVOIs() == null) || (matchImage.getVOIs().isEmpty() == true)) {
                    voiCheckbox.setEnabled(false);
                    voiCheckbox.setSelected(false);
                } else {
                    voiCheckbox.setEnabled(false);
                    voiCheckbox.setSelected(true);
                }

                labelTargetThreshold.setEnabled(false);
                textTargetThreshold.setEnabled(false);
            } else if (matchImage.getNDims() == 3) {
                doInternal = false;
                labelInternal.setEnabled(false);
                textInternal.setEnabled(false);
                adjacentButton.setEnabled(false);
                referenceButton.setEnabled(false);
                labelSourceGaussZ.setEnabled(true);
                textSourceGaussZ.setEnabled(true);
                labelTargetGaussX.setEnabled(true);
                textTargetGaussX.setEnabled(true);
                textTargetGaussX.setText("0.0");
                labelTargetGaussY.setEnabled(true);
                textTargetGaussY.setEnabled(true);
                textTargetGaussY.setText("0.0");
                labelTargetGaussZ.setEnabled(true);
                textTargetGaussZ.setEnabled(true);
                textTargetGaussZ.setText("0.0");
                targetVOICheckBox.setEnabled(true);

                if ((matchImage.getVOIs() == null) || (matchImage.getVOIs().isEmpty() == true)) {
                    voiCheckbox.setEnabled(false);
                    voiCheckbox.setSelected(false);
                } else {
                    voiCheckbox.setEnabled(true);
                    voiCheckbox.setSelected(false);
                }

                labelTargetThreshold.setEnabled(true);
                textTargetThreshold.setEnabled(true);
            }

            if (doInternal || (refImage.getVOIs() == null) || (refImage.getVOIs().isEmpty() == true)) {
                targetVOICheckBox.setEnabled(false);
                targetVOICheckBox.setSelected(false);
            } else {
                targetVOICheckBox.setEnabled(true);
            }
        } else if ((source == comboBoxInterp) && (clipCheckBox != null)) {
            selectedInterp = (String) comboBoxInterp.getSelectedItem();

            if (selectedInterp.equals("Windowed sinc")) {
                clipCheckBox.setEnabled(true);
            } else {
                clipCheckBox.setSelected(true);
                clipCheckBox.setEnabled(false);
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
        String image2Key = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
            image2Key = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        matchImage = im;
        UI = matchImage.getUserInterface();
        parentFrame = matchImage.getParentFrame();
        setRefImage(parser.getImage(image2Key));

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        try {
            setEntireTarget(parser.getNextBoolean());
            setEntireSource(parser.getNextBoolean());
            setTransformation(parser.getNextInteger());
            setInterpolation(parser.getNextInteger());
            setClipOutput(parser.getNextBoolean());
            setInteraction(parser.getNextBoolean());
            setTransformVOI(parser.getNextBoolean());
            setSourceThreshold(parser.getNextFloat());
            setTargetThreshold(parser.getNextFloat());
            setSourceGaussX(parser.getNextFloat());
            setSourceGaussY(parser.getNextFloat());
            setSourceGaussZ(parser.getNextFloat());
            setTargetGaussX(parser.getNextFloat());
            setTargetGaussY(parser.getNextFloat());
            setTargetGaussZ(parser.getNextFloat());
            setSampleFactor(parser.getNextInteger());
            setCostFxn(parser.getNextInteger());
            setPrecision(parser.getNextFloat());
            setIterations(parser.getNextInteger());
            setWriteParameters(parser.getNextBoolean());
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
     * Accessor to set whether the output range is clipped to the input range.
     *
     * @param  clipOutput  DOCUMENT ME!
     */
    public void setClipOutput(boolean clipOutput) {
        this.clipOutput = clipOutput;
    }

    /**
     * Accessor to set the cost function.
     *
     * @param  costFunction  DOCUMENT ME!
     */
    public void setCostFxn(int costFunction) {
        costFxn = costFunction;
    }

    /**
     * Accessor to set the entire target flag.
     *
     * @param  flag  <code>true</code> means use entire target image.
     */
    public void setEntireSource(boolean flag) {
        entireSource = flag;
    }

    /**
     * Accessor to set the entire target flag.
     *
     * @param  flag  <code>true</code> means use entire target image.
     */
    public void setEntireTarget(boolean flag) {
        entireTarget = flag;
    }

    /**
     * Accessor to set the interaction flag.
     *
     * @param  flag  <code>true</code> means interaction.
     */
    public void setInteraction(boolean flag) {
        interaction = flag;
    }

    /**
     * Accessor to set the type of interpolation for the registration.
     *
     * @param  interp  Type of interpolation (WSINC, etc)
     */
    public void setInterpolation(int interp) {
        interpolation = interp;
    }

    /**
     * Accessor to set the number of iterations.
     *
     * @param  iter  Number of iterations.
     */
    public void setIterations(int iter) {
        iterations = iter;
    }

    /**
     * Accessor to set the precision for the registration.
     *
     * @param  precise  Precision
     */
    public void setPrecision(float precise) {
        precision = precise;
    }

    /**
     * Accessor to set the target image.
     *
     * @param  image  The target image.
     */
    public void setRefImage(ModelImage image) {
        refImage = image;
    }

    /**
     * Accessor to set the sample factor.
     *
     * @param  samp  Sample factor.
     */
    public void setSampleFactor(int samp) {
        sampleFactor2 = samp;
    }

    /**
     * Accessor to set the source Gaussian x for the registration.
     *
     * @param  x  Source Gaussian X parameter.
     */
    public void setSourceGaussX(float x) {
        sourceGaussX = x;
    }

    /**
     * Accessor to set the source Gaussian y for the registration.
     *
     * @param  x  Source Gaussian Y parameter.
     */
    public void setSourceGaussY(float x) {
        sourceGaussY = x;
    }

    /**
     * Accessor to set the source Gaussian z for the registration.
     *
     * @param  x  Source Gaussian Z parameter.
     */
    public void setSourceGaussZ(float x) {
        sourceGaussZ = x;
    }

    /**
     * Accessor to set the source threshold for the registration.
     *
     * @param  thresh  Source threshold
     */
    public void setSourceThreshold(float thresh) {
        sourceThreshold = thresh;
    }

    /**
     * Accessor to set the target Gaussian x for the registration.
     *
     * @param  x  Target Gaussian X parameter.
     */
    public void setTargetGaussX(float x) {
        targetGaussX = x;
    }

    /**
     * Accessor to set the target Gaussian y for the registration.
     *
     * @param  x  Target Gaussian Y parameter.
     */
    public void setTargetGaussY(float x) {
        targetGaussY = x;
    }

    /**
     * Accessor to set the target Gaussian z for the registration.
     *
     * @param  x  Target Gaussian Z parameter.
     */
    public void setTargetGaussZ(float x) {
        targetGaussZ = x;
    }

    /**
     * Accessor to set the target threshold for the registration.
     *
     * @param  thresh  Target threshold
     */
    public void setTargetThreshold(float thresh) {
        targetThreshold = thresh;
    }

    /**
     * Accessor to set the type of transformation for the registration.
     *
     * @param  trans  Type of transformation (GLOBAL_RESCALING2D, etc)
     */
    public void setTransformation(int trans) {
        transformation = trans;
    }

    /**
     * Accessor to set whether the VOI is transformed.
     *
     * @param  transformVOI  DOCUMENT ME!
     */
    public void setTransformVOI(boolean transformVOI) {
        this.transformVOI = transformVOI;
    }

    /**
     * Accessor to set whether or not the calculated parameters are written to a file.
     *
     * @param  writeParameters  DOCUMENT ME!
     */
    public void setWriteParameters(boolean writeParameters) {
        this.writeParameters = writeParameters;
    }

    /**
     * Builds a list of images. Returns combobox.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    protected JComboBox buildImgComboBox(ModelImage image) {
        ViewUserInterface UI;
        ModelImage img;

        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        UI = image.getUserInterface();

        Enumeration names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();
            img = UI.getRegisteredImageByName(name);

            if (UI.getFrameContainingImage(img) != null) {

                if (!name.equals(image.getImageName())) {
                    comboBox.addItem(name);
                }
            }
        }

        if (image.getNDims() == 3) {
            comboBox.addItem(image.getImageName());
        }

        return comboBox;
    }

    /**
     * Runs the algorithm.
     */
    private void callAlgorithm() {

        if (!doInternal) {

            try {
                targetXDim = refImage.getExtents()[0];
                targetYDim = refImage.getExtents()[1];

                if (refImage.getNDims() > 2) {
                    targetZDim = refImage.getExtents()[2];
                }

                if (refImage.getNDims() == 2) {
                    resultExtents = new int[2];
                    resultExtents[0] = targetXDim;
                    resultExtents[1] = targetYDim;
                } else {
                    resultExtents = new int[3];
                    resultExtents[0] = targetXDim;
                    resultExtents[1] = targetYDim;
                    resultExtents[2] = targetZDim;
                }

                resultImage = new ModelImage(ModelStorageBase.FLOAT, resultExtents,
                                             makeImageName(matchImage.getImageName(), "_registered"), UI);

                nl = new AlgorithmRegNonlinear(resultImage, refImage, entireTarget, matchImage, entireSource,
                                               transformation, interpolation, clipOutput, interaction, transformVOI,
                                               sourceThreshold, targetThreshold, sourceGaussX, sourceGaussY,
                                               sourceGaussZ, targetGaussX, targetGaussY, targetGaussZ, sampleFactor2,
                                               costFxn, precision, iterations, writeParameters, esInitial);

            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog RegistrationNonlinear: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                dispose();

                return;
            }

            // This is very important. Adding this object as a listener allows
            // the algorithm to notify this object when it has completed or failed.
            // See algorithm performed event. This is made possible by implementing
            nl.addListener(this);

            if (runInSeparateThread) {

                // Start the thread as a low priority because we wish to still have
                // user interface work fast
                if (nl.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                nl.setActiveImage(isActiveImage);

                if (!UI.isAppFrameVisible()) {
                    nl.setProgressBarVisible(false);
                }

                nl.run();
            }
        } // if (!doInternal)
        else { // doInternal

            try {
                resultImage = new ModelImage(ModelStorageBase.FLOAT, matchImage.getExtents(),
                                             makeImageName(matchImage.getImageName(), "_registered"), UI);

                nl = new AlgorithmRegNonlinear(resultImage, referenceSlice, doAdjacent, entireTarget, matchImage,
                                               entireSource, transformation, interpolation, clipOutput, interaction,
                                               transformVOI, sourceThreshold, targetThreshold, sourceGaussX,
                                               sourceGaussY, sourceGaussZ, targetGaussX, targetGaussY, targetGaussZ,
                                               sampleFactor2, costFxn, precision, iterations, writeParameters);
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog RegistrationNonlinear: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                dispose();

                return;
            }

            // This is very important. Adding this object as a listener allows
            // the algorithm to notify this object when it has completed or failed.
            // See algorithm performed event. This is made possible by implementing
            nl.addListener(this);

            if (runInSeparateThread) {

                // Start the thread as a low priority because we wish to still have
                // user interface work fast
                if (nl.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                nl.setActiveImage(isActiveImage);

                if (!UI.isAppFrameVisible()) {
                    nl.setProgressBarVisible(false);
                }

                nl.run();
            }
        } // else doInternal

    }

    /**
     * Initializes the GUI components and displays the dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Nonlinear Registration");

        sourceName = matchImage.getImageName();

        JLabel labelImage = new JLabel("Register [" + sourceName + "] to:");
        labelImage.setForeground(Color.black);
        labelImage.setBounds(10, 20, 230, 25);
        labelImage.setFont(serif12);
        labelImage.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxImage = buildImgComboBox(matchImage);
        comboBoxImage.addItemListener(this);

        selectedName = (String) comboBoxImage.getSelectedItem();
        refImage = UI.getRegisteredImageByName(selectedName);

        if (matchImage.getNDims() == 3) {
            labelInternal = new JLabel("Reference slice (1-" + String.valueOf(matchImage.getExtents()[2]) + ")");
            labelInternal.setForeground(Color.black);
            labelInternal.setBounds(10, 20, 230, 25);
            labelInternal.setFont(serif12);
            labelInternal.setAlignmentX(Component.LEFT_ALIGNMENT);
            textInternal = new JTextField(10);
            textInternal.setText(String.valueOf((matchImage.getExtents()[2] / 2) + 1));
            textInternal.setFont(serif12);

            if (sourceName == selectedName) {
                doInternal = true;
                labelInternal.setEnabled(true);
                textInternal.setEnabled(true);
            } else {
                doInternal = false;
                labelInternal.setEnabled(false);
                textInternal.setEnabled(false);
            }

            internalGroup = new ButtonGroup();
            adjacentButton = new JRadioButton("Register to adjacent slice", true);
            adjacentButton.setFont(serif12);
            adjacentButton.setForeground(Color.black);
            internalGroup.add(adjacentButton);

            if (doInternal) {
                adjacentButton.setEnabled(true);
            } else {
                adjacentButton.setEnabled(false);
            }

            referenceButton = new JRadioButton("Register to reference slice", false);
            referenceButton.setFont(serif12);
            referenceButton.setForeground(Color.black);
            internalGroup.add(referenceButton);

            if (doInternal) {
                referenceButton.setEnabled(true);
            } else {
                referenceButton.setEnabled(false);
            }
        } // if (matchImage.getNDims() == 3)

        JLabel labelDOF = new JLabel("Degrees of freedom:");
        labelDOF.setForeground(Color.black);
        labelDOF.setFont(serif12);
        labelDOF.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxDOF = new JComboBox();
        comboBoxDOF.setFont(MipavUtil.font12);
        comboBoxDOF.setBackground(Color.white);
        comboBoxDOF.setToolTipText("Degrees of freedom");

        if ((matchImage.getNDims() == 2) || doInternal) {
            comboBoxDOF.addItem("First order linear - 6");
            comboBoxDOF.addItem("Second order nonlinear - 12");
            comboBoxDOF.addItem("Third order nonlinear - 20");
            comboBoxDOF.addItem("Fourth order nonlinear - 30");
            comboBoxDOF.addItem("Fifth order nonlinear - 42");
            comboBoxDOF.addItem("Sixth order nonlinear - 56");
            comboBoxDOF.addItem("Seventh order nonlinear - 72");
            comboBoxDOF.addItem("Eighth order nonlinear - 90");
            comboBoxDOF.addItem("Ninth order nonlinear - 110");
            comboBoxDOF.addItem("Tenth order nonlinear - 132");
            comboBoxDOF.addItem("Eleventh order nonlinear - 156");
            comboBoxDOF.addItem("Twelfth order nonlinear - 182");
        } else {
            comboBoxDOF.addItem("First order linear - 12");
            comboBoxDOF.addItem("Second order nonlinear - 30");
            comboBoxDOF.addItem("Third order nonlinear - 60");
            comboBoxDOF.addItem("Fourth order nonlinear - 105");
            comboBoxDOF.addItem("Fifth order nonlinear - 168");
            comboBoxDOF.addItem("Sixth order nonlinear - 252");
            comboBoxDOF.addItem("Seventh order nonlinear - 360");
            comboBoxDOF.addItem("Eighth order nonlinear - 495");
            comboBoxDOF.addItem("Ninth order nonlinear - 660");
            comboBoxDOF.addItem("Tenth order nonlinear - 858");
            comboBoxDOF.addItem("Eleventh order nonliear - 1092");
            comboBoxDOF.addItem("Twelfth order nonlinear - 1365");
        }

        JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);
        comboBoxInterp.addItemListener(this);

        comboBoxInterp.addItem("Linear");
        comboBoxInterp.addItem("Windowed sinc");
        comboBoxInterp.addItem("Nearest Neighbor");

        JLabel labelCost = new JLabel("Cost function:");
        labelCost.setForeground(Color.black);
        labelCost.setFont(serif12);
        labelCost.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxCostFunct = new JComboBox();
        comboBoxCostFunct.setFont(MipavUtil.font12);
        comboBoxCostFunct.setBackground(Color.white);
        comboBoxCostFunct.setToolTipText("Cost function");
        comboBoxCostFunct.addItem("Scaled least squares");
        comboBoxCostFunct.addItem("Correlation ratio");
        comboBoxCostFunct.setSelectedIndex(0);

        clipCheckBox = new JCheckBox("Clip output values to input range");
        clipCheckBox.setFont(serif12);
        clipCheckBox.setEnabled(false);
        clipCheckBox.setSelected(true);
        clipCheckBox.addActionListener(this);

        interactionCheckBox = new JCheckBox("Spatial parameter derivatives interact");
        interactionCheckBox.setFont(serif12);
        interactionCheckBox.setEnabled(true);
        interactionCheckBox.setSelected(true);
        interactionCheckBox.addActionListener(this);

        voiCheckbox = new JCheckBox("Transform source VOIs");
        voiCheckbox.setFont(serif12);
        voiCheckbox.setSelected(false);
        voiCheckbox.addActionListener(this);

        // Should be transform VOI also
        if ((matchImage.getVOIs() == null) || (matchImage.getVOIs().isEmpty() == true)) {
            voiCheckbox.setEnabled(false);
            voiCheckbox.setSelected(false);
        } else if (doInternal) {
            voiCheckbox.setEnabled(false);
            voiCheckbox.setSelected(true);
        } else {
            voiCheckbox.setEnabled(true);
            voiCheckbox.setSelected(false);
        }

        JPanel optPanel = new JPanel(new GridBagLayout());
        optPanel.setBorder(buildTitledBorder("Transformation Options"));

        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        gbc.anchor = gbc.WEST;
        optPanel.add(labelImage, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        optPanel.add(comboBoxImage, gbc);

        if (matchImage.getNDims() == 3) {
            addInternal = 3;
            gbc.gridx = 0;
            gbc.gridy = 2;
            gbc.weightx = 0;
            gbc.fill = gbc.NONE;
            optPanel.add(labelInternal, gbc);
            gbc.gridx = 1;
            gbc.gridy = 2;
            gbc.weightx = 1;
            gbc.fill = gbc.HORIZONTAL;
            optPanel.add(textInternal, gbc);
            gbc.gridx = 0;
            gbc.gridy = 3;
            gbc.weightx = 1;
            gbc.fill = gbc.HORIZONTAL;
            optPanel.add(adjacentButton, gbc);
            gbc.gridx = 0;
            gbc.gridy = 4;
            gbc.weightx = 1;
            gbc.fill = gbc.HORIZONTAL;
            optPanel.add(referenceButton, gbc);
        }

        gbc.gridx = 0;
        gbc.gridy = 2 + addInternal;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        optPanel.add(labelDOF, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2 + addInternal;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        optPanel.add(comboBoxDOF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3 + addInternal;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        optPanel.add(labelInterp, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3 + addInternal;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        optPanel.add(comboBoxInterp, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4 + addInternal;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        optPanel.add(labelCost, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4 + addInternal;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        optPanel.add(comboBoxCostFunct, gbc);

        gbc.gridx = 0;
        gbc.gridy = 5 + addInternal;
        gbc.weightx = 1;
        gbc.gridwidth = 2;
        optPanel.add(clipCheckBox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 6 + addInternal;
        gbc.weightx = 1;
        gbc.gridwidth = 2;
        optPanel.add(interactionCheckBox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 7 + addInternal;
        gbc.weightx = 1;
        gbc.gridwidth = 2;
        optPanel.add(voiCheckbox, gbc);

        sourceVOICheckBox = new JCheckBox("Apply only to VOI region in source");
        sourceVOICheckBox.setFont(serif12);

        if ((matchImage.getVOIs() == null) || (matchImage.getVOIs().isEmpty() == true)) {
            sourceVOICheckBox.setEnabled(false);
        } else {
            sourceVOICheckBox.setEnabled(true);
        }

        sourceVOICheckBox.setSelected(false);
        sourceVOICheckBox.addActionListener(this);

        targetVOICheckBox = new JCheckBox("Apply only to VOI region in destination");
        targetVOICheckBox.setFont(serif12);

        if (doInternal || (refImage.getVOIs() == null) || (refImage.getVOIs().isEmpty() == true)) {
            targetVOICheckBox.setEnabled(false);
        } else {
            targetVOICheckBox.setEnabled(true);
        }

        targetVOICheckBox.setSelected(false);
        targetVOICheckBox.addActionListener(this);

        JPanel VOIPanel = new JPanel(new GridBagLayout());
        VOIPanel.setBorder(buildTitledBorder("VOI region use"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.anchor = gbc.WEST;
        VOIPanel.add(sourceVOICheckBox, gbc);
        gbc.gridy = 1;
        VOIPanel.add(targetVOICheckBox, gbc);

        JLabel labelInterval = new JLabel("Final sampling interval");
        labelInterval.setForeground(Color.black);
        labelInterval.setFont(serif12);

        textFinalSampling = new JTextField(10);
        textFinalSampling.setText("1");
        textFinalSampling.setFont(serif12);

        JLabel labelPrecision = new JLabel("Convergence criteria(0.00001-0.5)");
        labelPrecision.setForeground(Color.black);
        labelPrecision.setFont(serif12);

        textPrecision = new JTextField(10);
        textPrecision.setText("0.001");
        textPrecision.setFont(serif12);

        JLabel labelIterations = new JLabel("Maximum Iterations");
        labelIterations.setForeground(Color.black);
        labelIterations.setFont(serif12);

        textIterations = new JTextField(10);
        textIterations.setText("50");
        textIterations.setFont(serif12);

        JPanel iterationsPanel = new JPanel(new GridBagLayout());
        iterationsPanel.setBorder(buildTitledBorder("Iteration parameters"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        gbc.anchor = gbc.WEST;
        iterationsPanel.add(labelPrecision, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        iterationsPanel.add(textPrecision, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        iterationsPanel.add(labelIterations, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        iterationsPanel.add(textIterations, gbc);

        JLabel labelSourceThreshold = new JLabel("Source threshold (" + nf.format(matchImage.getMin()) + " - " +
                                                 nf.format(matchImage.getMax()) + ")");
        labelSourceThreshold.setForeground(Color.black);
        labelSourceThreshold.setFont(serif12);

        textSourceThreshold = new JTextField(10);
        sourceThreshold = (float) (matchImage.getMin() + (0.2f * (matchImage.getMax() - matchImage.getMin())));
        textSourceThreshold.setText(nf.format(sourceThreshold));
        textSourceThreshold.setFont(serif12);

        targetThreshold = (float) (refImage.getMin() + (0.2f * (refImage.getMax() - refImage.getMin())));

        labelTargetThreshold = new JLabel("Target threshold (" + nf.format(refImage.getMin()) + " - " +
                                          nf.format(refImage.getMax()) + ")");
        labelTargetThreshold.setForeground(Color.black);
        labelTargetThreshold.setFont(serif12);

        if (doInternal) {
            labelTargetThreshold.setEnabled(false);
        } else {
            labelTargetThreshold.setEnabled(true);
        }

        textTargetThreshold = new JTextField(10);

        if (doInternal) {
            textTargetThreshold.setText(" ");
        } else {
            textTargetThreshold.setText(nf.format(targetThreshold));
        }

        textTargetThreshold.setFont(serif12);

        if (doInternal) {
            textTargetThreshold.setEnabled(false);
        } else {
            textTargetThreshold.setEnabled(true);
        }

        JPanel thresholdPanel = new JPanel(new GridBagLayout());
        thresholdPanel.setBorder(buildTitledBorder("Thresholds"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 1;
        gbc.fill = gbc.NONE;
        gbc.anchor = gbc.WEST;
        thresholdPanel.add(labelSourceThreshold, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        thresholdPanel.add(textSourceThreshold, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        thresholdPanel.add(labelTargetThreshold, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        thresholdPanel.add(textTargetThreshold, gbc);

        JLabel labelSourceGaussX = new JLabel("Source X FWHM");
        labelSourceGaussX.setForeground(Color.black);
        labelSourceGaussX.setFont(serif12);

        textSourceGaussX = new JTextField(10);
        textSourceGaussX.setText("0.0");
        textSourceGaussX.setFont(serif12);

        JLabel labelSourceGaussY = new JLabel("Source Y FWHM");
        labelSourceGaussY.setForeground(Color.black);
        labelSourceGaussY.setFont(serif12);

        textSourceGaussY = new JTextField(10);
        textSourceGaussY.setText("0.0");
        textSourceGaussY.setFont(serif12);

        labelSourceGaussZ = new JLabel("Source Z FWHM");
        labelSourceGaussZ.setForeground(Color.black);
        labelSourceGaussZ.setFont(serif12);

        if (doInternal) {
            labelSourceGaussZ.setEnabled(false);
        }

        textSourceGaussZ = new JTextField(10);
        textSourceGaussZ.setText("0.0");
        textSourceGaussZ.setFont(serif12);

        if (doInternal) {
            textSourceGaussZ.setEnabled(false);
        }

        labelTargetGaussX = new JLabel("Target X FWHM");
        labelTargetGaussX.setForeground(Color.black);
        labelTargetGaussX.setFont(serif12);

        if (doInternal) {
            labelTargetGaussX.setEnabled(false);
        }

        textTargetGaussX = new JTextField(10);

        if (doInternal) {
            textTargetGaussX.setText(" ");
        } else {
            textTargetGaussX.setText("0.0");
        }

        textTargetGaussX.setFont(serif12);

        if (doInternal) {
            textTargetGaussX.setEnabled(false);
        }

        labelTargetGaussY = new JLabel("Target Y FWHM");
        labelTargetGaussY.setForeground(Color.black);
        labelTargetGaussY.setFont(serif12);

        if (doInternal) {
            labelTargetGaussY.setEnabled(false);
        }

        textTargetGaussY = new JTextField(10);

        if (doInternal) {
            textTargetGaussY.setText(" ");
        } else {
            textTargetGaussY.setText("0.0");
        }

        textTargetGaussY.setFont(serif12);

        if (doInternal) {
            textTargetGaussY.setEnabled(false);
        }

        labelTargetGaussZ = new JLabel("Target Z FWHM");
        labelTargetGaussZ.setForeground(Color.black);
        labelTargetGaussZ.setFont(serif12);

        if (doInternal) {
            labelTargetGaussZ.setEnabled(false);
        }

        textTargetGaussZ = new JTextField(10);

        if (doInternal) {
            textTargetGaussZ.setText(" ");
        } else {
            textTargetGaussZ.setText("0.0");
        }

        textTargetGaussZ.setFont(serif12);

        if (doInternal) {
            textTargetGaussZ.setEnabled(false);
        }

        JPanel gaussianPanel = new JPanel(new GridBagLayout());
        gaussianPanel.setBorder(buildTitledBorder("Gaussian FWHMs"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        gbc.anchor = gbc.WEST;
        gaussianPanel.add(labelSourceGaussX, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gaussianPanel.add(textSourceGaussX, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        gaussianPanel.add(labelSourceGaussY, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gaussianPanel.add(textSourceGaussY, gbc);

        if (matchImage.getNDims() > 2) {
            gbc.gridx = 0;
            gbc.gridy = 2;
            gbc.weightx = 0;
            gbc.fill = gbc.NONE;
            gaussianPanel.add(labelSourceGaussZ, gbc);
            gbc.gridx = 1;
            gbc.gridy = 2;
            gbc.weightx = 1;
            gbc.fill = gbc.HORIZONTAL;
            gaussianPanel.add(textSourceGaussZ, gbc);
        }

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        gaussianPanel.add(labelTargetGaussX, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gaussianPanel.add(textTargetGaussX, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        gaussianPanel.add(labelTargetGaussY, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gaussianPanel.add(textTargetGaussY, gbc);

        if (matchImage.getNDims() > 2) {
            gbc.gridx = 0;
            gbc.gridy = 5;
            gbc.weightx = 0;
            gbc.fill = gbc.NONE;
            gaussianPanel.add(labelTargetGaussZ, gbc);
            gbc.gridx = 1;
            gbc.gridy = 5;
            gbc.weightx = 1;
            gbc.fill = gbc.HORIZONTAL;
            gaussianPanel.add(textTargetGaussZ, gbc);
        }

        parameterCheckBox = new JCheckBox("Write " + sourceName + ".par");
        parameterCheckBox.setFont(serif12);
        parameterCheckBox.setEnabled(true);
        parameterCheckBox.setSelected(false);
        parameterCheckBox.addActionListener(this);

        JPanel parameterPanel = new JPanel(new GridBagLayout());
        parameterPanel.setBorder(buildTitledBorder("Save parameters to file"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        gbc.anchor = gbc.WEST;
        parameterPanel.add(parameterCheckBox, gbc);

        JPanel leftPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 1;
        gbc.fill = gbc.BOTH;
        gbc.anchor = gbc.WEST;

        leftPanel.add(optPanel, gbc);
        gbc.gridy = 1;
        leftPanel.add(VOIPanel, gbc);
        gbc.gridy = 2;
        leftPanel.add(iterationsPanel, gbc);

        JPanel rightPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = gbc.BOTH;
        gbc.anchor = gbc.WEST;

        rightPanel.add(thresholdPanel, gbc);
        gbc.gridy = 1;
        rightPanel.add(gaussianPanel, gbc);
        gbc.gridy = 2;
        rightPanel.add(parameterPanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buildCancelButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
        getContentPane().add(leftPanel);
        getContentPane().add(rightPanel, BorderLayout.EAST);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Sets up the variables needed for the algorithm from the GUI components.
     *
     * @return  Flag indicating if the setup was successful.
     */
    private boolean setVariables() {

        // assign source Image to image selected in comboBox
        selectedName = (String) comboBoxImage.getSelectedItem();
        refImage = UI.getRegisteredImageByName(selectedName);

        if (doInternal) {
            tmpStr = textInternal.getText();

            if (testParameter(tmpStr, 1.0, matchImage.getExtents()[2])) {
                referenceSlice = Integer.valueOf(tmpStr).intValue() - 1;
            } else {
                textInternal.requestFocus();
                textInternal.selectAll();

                return false;
            }

            if (adjacentButton.isSelected()) {
                doAdjacent = true;
            } else {
                doAdjacent = false;
            }
        } // if (doInternal)

        switch (comboBoxDOF.getSelectedIndex()) {

            case 1:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.NL2D12;
                } else {
                    transformation = AlgorithmRegNonlinear.NL3D30;
                }

                break;

            case 2:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.NL2D20;
                } else {
                    transformation = AlgorithmRegNonlinear.NL3D60;
                }

                break;

            case 3:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.NL2D30;
                } else {
                    transformation = AlgorithmRegNonlinear.NL3D105;
                }

                break;

            case 4:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.NL2D42;
                } else {
                    transformation = AlgorithmRegNonlinear.NL3D168;
                }

                break;

            case 5:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.NL2D56;
                } else {
                    transformation = AlgorithmRegNonlinear.NL3D252;
                }

                break;

            case 6:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.NL2D72;
                } else {
                    transformation = AlgorithmRegNonlinear.NL3D360;
                }

                break;

            case 7:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.NL2D90;
                } else {
                    transformation = AlgorithmRegNonlinear.NL3D495;
                }

                break;

            case 8:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.NL2D110;
                } else {
                    transformation = AlgorithmRegNonlinear.NL3D660;
                }

                break;

            case 9:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.NL2D132;
                } else {
                    transformation = AlgorithmRegNonlinear.NL3D858;
                }

                break;

            case 10:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.NL2D156;
                } else {
                    transformation = AlgorithmRegNonlinear.NL3D1092;
                }

                break;

            case 11:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.NL2D182;
                } else {
                    transformation = AlgorithmRegNonlinear.NL3D1365;
                }

                break;

            case 0:
            default:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    transformation = AlgorithmRegNonlinear.LINEAR2D6;
                } else {
                    transformation = AlgorithmRegNonlinear.LINEAR3D12;
                }

                break;
        }

        switch (comboBoxInterp.getSelectedIndex()) {

            case 0:
                if ((matchImage.getNDims() == 2) || (doInternal)) {
                    interpolation = AlgorithmTransform.BILINEAR;
                } else {
                    interpolation = AlgorithmTransform.TRILINEAR;
                }

                break;

            case 1:
                interpolation = AlgorithmTransform.WSINC;
                break;

            case 2:
                interpolation = AlgorithmTransform.NEAREST_NEIGHBOR;
                break;

            default:
                interpolation = AlgorithmTransform.TRILINEAR;
                break;
        }

        switch (comboBoxCostFunct.getSelectedIndex()) {

            case 0:
                costFxn = AlgorithmRegNonlinear.SCALED_LEAST_SQUARES;
                break;

            case 1:
                costFxn = AlgorithmRegNonlinear.CORRELATION_RATIO;
                break;

            default:
                costFxn = AlgorithmRegNonlinear.SCALED_LEAST_SQUARES;
        }

        if (interactionCheckBox.isSelected()) {
            interaction = true;
        } else {
            interaction = false;
        }

        if (clipCheckBox.isSelected()) {
            clipOutput = true;
        } else {
            clipOutput = false;
        }

        if (voiCheckbox.isSelected()) {
            transformVOI = true;
        } else {
            transformVOI = false;
        }

        tmpStr = textSourceThreshold.getText();

        if (testParameter(tmpStr, matchImage.getMin(), matchImage.getMax())) {
            sourceThreshold = Float.valueOf(tmpStr).floatValue();
        } else {
            textSourceThreshold.requestFocus();
            textSourceThreshold.selectAll();

            return false;
        }

        if (doInternal) {
            targetThreshold = sourceThreshold;
        } else {
            tmpStr = textTargetThreshold.getText();

            if (testParameter(tmpStr, refImage.getMin(), refImage.getMax())) {
                targetThreshold = Float.valueOf(tmpStr).floatValue();
            } else {
                textTargetThreshold.requestFocus();
                textTargetThreshold.selectAll();

                return false;
            }
        }

        tmpStr = textSourceGaussX.getText();

        if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
            sourceGaussX = Float.valueOf(tmpStr).floatValue();
        } else {
            textSourceGaussX.requestFocus();
            textSourceGaussX.selectAll();

            return false;
        }

        tmpStr = textSourceGaussY.getText();

        if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
            sourceGaussY = Float.valueOf(tmpStr).floatValue();
        } else {
            textSourceGaussY.requestFocus();
            textSourceGaussY.selectAll();

            return false;
        }

        tmpStr = textSourceGaussZ.getText();

        if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
            sourceGaussZ = Float.valueOf(tmpStr).floatValue();
        } else {
            textSourceGaussZ.requestFocus();
            textSourceGaussZ.selectAll();

            return false;
        }

        if (doInternal) {
            targetGaussX = sourceGaussX;
        } else {
            tmpStr = textTargetGaussX.getText();

            if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
                targetGaussX = Float.valueOf(tmpStr).floatValue();
            } else {
                textTargetGaussX.requestFocus();
                textTargetGaussX.selectAll();

                return false;
            }
        }

        if (doInternal) {
            targetGaussY = sourceGaussY;
        } else {
            tmpStr = textTargetGaussY.getText();

            if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
                targetGaussY = Float.valueOf(tmpStr).floatValue();
            } else {
                textTargetGaussY.requestFocus();
                textTargetGaussY.selectAll();

                return false;
            }
        }

        if (doInternal) {
            targetGaussZ = sourceGaussZ;
        } else {
            tmpStr = textTargetGaussZ.getText();

            if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
                targetGaussZ = Float.valueOf(tmpStr).floatValue();
            } else {
                textTargetGaussZ.requestFocus();
                textTargetGaussZ.selectAll();

                return false;
            }
        }

        tmpStr = textFinalSampling.getText();

        if (testParameter(tmpStr, 1.0, 81.0)) {
            sampleFactor2 = Integer.valueOf(tmpStr).intValue();
        } else {
            textFinalSampling.requestFocus();
            textFinalSampling.selectAll();

            return false;
        }

        tmpStr = textPrecision.getText();

        if (testParameter(tmpStr, 0.00001, 0.5)) {
            precision = Float.valueOf(tmpStr).floatValue();
        } else {
            textPrecision.requestFocus();
            textPrecision.selectAll();

            return false;
        }

        tmpStr = textIterations.getText();

        if (testParameter(tmpStr, 1, 10000)) {
            iterations = Integer.valueOf(tmpStr).intValue();
        } else {
            textIterations.requestFocus();
            textIterations.selectAll();

            return false;
        }

        if (sourceVOICheckBox.isSelected()) {
            entireSource = false;
        } else {
            entireSource = true;
        }

        if (doInternal) {
            entireTarget = entireSource;
        } else if (targetVOICheckBox.isSelected()) {
            entireTarget = false;
        } else {
            entireTarget = true;
        }

        if (parameterCheckBox.isSelected()) {
            writeParameters = true;
        } else {
            writeParameters = false;
        }

        return true;
    }
}
