package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call a specified diffusion algorithm. It should be noted that the algorithms are
 * executed in their own thread.
 */
public class JDialogCoherenceEnhancingDiffusion extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1141466038513737001L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox checkBox25D;

    /** DOCUMENT ME! */
    private AlgorithmCoherenceEnhancingDiffusion coherenceEnhancingDiffusionAlgo = null;

    /** DOCUMENT ME! */
    private float derivativeScale = 0.5f;

    /** DOCUMENT ME! */
    private float diffusitivityDenom = 0.001f;

    /** DOCUMENT ME! */
    private boolean do25D = true;

    /** DOCUMENT ME! */
    private boolean entireImage = true;

    /** DOCUMENT ME! */
    private float gaussianScale = 2.0f;

    /** DOCUMENT ME! */
    private int numIterations = 1;

    /** DOCUMENT ME! */
    private JRadioButton radioEntireImage, radioVOIRegion;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private JTextField textDerivative, textGaussian;

    /** DOCUMENT ME! */
    private JTextField textNumberIterations, textDiffusitivity;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogCoherenceEnhancingDiffusion() { }

    /**
     * Initialize the dialog.
     *
     * @param  frame  the parent frame
     * @param  im     the source image
     */
    public JDialogCoherenceEnhancingDiffusion(Frame frame, ModelImage im) {
        super(frame, false);

        srcImage = im;
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

        if (((algorithm instanceof AlgorithmCoherenceEnhancingDiffusion) &&
                 (coherenceEnhancingDiffusionAlgo.isCompleted() == true)) && (resultImage != null)) {

            updateFileInfo(srcImage, resultImage);
            resultImage.clearMask();

            try {
                new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            insertScriptLine();
         // save the completion status for later
            setComplete(algorithm.isCompleted());

            coherenceEnhancingDiffusionAlgo.finalize();
            coherenceEnhancingDiffusionAlgo = null;
        }
    } // end algorithmPerformed(...)

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Accessor that sets the derivativeScale.
     *
     * @param  derivativeScale  DOCUMENT ME!
     */
    public void setDerivativeScale(float derivativeScale) {
        this.derivativeScale = derivativeScale;
    }

    /**
     * Accessor that sets the diffusitivityDenom.
     *
     * @param  diffusitivityDenom  DOCUMENT ME!
     */
    public void setDiffusitivityDenom(float diffusitivityDenom) {
        this.diffusitivityDenom = diffusitivityDenom;
    }

    /**
     * Accessor that sets if slice by slice processing occurs.
     *
     * @param  do25D  DOCUMENT ME!
     */
    public void setDo25D(boolean do25D) {
        this.do25D = do25D;
    }

    /**
     * Accessor that sets the gaussian scale.
     *
     * @param  gaussianScale  DOCUMENT ME!
     */
    public void setGaussianScale(float gaussianScale) {
        this.gaussianScale = gaussianScale;
    }

    /**
     * Accessor that sets the number of iterations.
     *
     * @param  numIterations  DOCUMENT ME!
     */
    public void setNumIterations(int numIterations) {
        this.numIterations = numIterations;
    }

    /**
     * Once all the necessary variables are set, call the mean algorithm based on what type of image this is and whether
     * or not there is a separate destination image.
     */
    protected void callAlgorithm() {


        String name;

        name = makeImageName(srcImage.getImageName(), "_ce");

        try {

            if (srcImage.isColorImage()) {
                resultImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), name);
            } else {
                resultImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), name);
            }

            coherenceEnhancingDiffusionAlgo = new AlgorithmCoherenceEnhancingDiffusion(resultImage, srcImage,
                                                                                       numIterations,
                                                                                       diffusitivityDenom,
                                                                                       derivativeScale, gaussianScale,
                                                                                       do25D, entireImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            coherenceEnhancingDiffusionAlgo.addListener(this);

            createProgressBar(srcImage.getImageName(), coherenceEnhancingDiffusionAlgo);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (coherenceEnhancingDiffusionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                coherenceEnhancingDiffusionAlgo.run();
            } // end if (isRunInSeparateThread())

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("JDialogCohEnhDiffusion: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        } // end try()=catch()

        dispose();
    } // end callAlgorithm()

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        srcImage = scriptParameters.retrieveInputImage();
        parentFrame = srcImage.getParentFrame();

        do25D = scriptParameters.doProcess3DAs25D();
        entireImage = scriptParameters.doProcessWholeImage();
        numIterations = scriptParameters.getNumIterations();
        gaussianScale = scriptParameters.getParams().getFloat("gaussian_scale");
        derivativeScale = scriptParameters.getParams().getFloat("derivative_scale");
        diffusitivityDenom = scriptParameters.getParams().getFloat("diffusitivity_denominator");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);
        scriptParameters.storeImageInRecorder(getResultImage());

        scriptParameters.storeProcess3DAs25D(do25D);
        scriptParameters.storeProcessWholeImage(entireImage);
        scriptParameters.storeNumIterations(numIterations);
        scriptParameters.getParams().put(ParameterFactory.newParameter("gaussian_scale", gaussianScale));
        scriptParameters.getParams().put(ParameterFactory.newParameter("derivative_scale", derivativeScale));
        scriptParameters.getParams().put(ParameterFactory.newParameter("diffusitivity_denominator",
                                                                       diffusitivityDenom));
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildParameterPanel() {
        JPanel parameterPanel = new JPanel(new GridBagLayout());

        parameterPanel.setForeground(Color.black);
        parameterPanel.setBorder(buildTitledBorder("Parameters"));

        JLabel labelNumberIterations = createLabel("Number of Iterations");

        textNumberIterations = new JTextField();
        textNumberIterations.setText(String.valueOf(numIterations));
        textNumberIterations.setColumns(5);
        textNumberIterations.setMaximumSize(textNumberIterations.getPreferredSize());
        textNumberIterations.setHorizontalAlignment(JTextField.RIGHT);
        textNumberIterations.setFont(serif12);

        JLabel labelDiffusitivity = createLabel("Diffusitivity denominator");

        textDiffusitivity = new JTextField();
        textDiffusitivity.setText(String.valueOf(diffusitivityDenom));
        textDiffusitivity.setColumns(5);
        textDiffusitivity.setMaximumSize(textDiffusitivity.getPreferredSize());
        textDiffusitivity.setHorizontalAlignment(JTextField.RIGHT);
        textDiffusitivity.setFont(serif12);

        JLabel labelDerivative = createLabel("Derivative scale space");

        textDerivative = new JTextField();
        textDerivative.setText(String.valueOf(derivativeScale));
        textDerivative.setColumns(5);
        textDerivative.setMaximumSize(textDerivative.getPreferredSize());
        textDerivative.setHorizontalAlignment(JTextField.RIGHT);
        textDerivative.setFont(serif12);

        JLabel labelGaussian = createLabel("Gaussian scale space");

        textGaussian = new JTextField();
        textGaussian.setText(String.valueOf(gaussianScale));
        textGaussian.setColumns(5);
        textGaussian.setMaximumSize(textGaussian.getPreferredSize());
        textGaussian.setHorizontalAlignment(JTextField.RIGHT);
        textGaussian.setFont(serif12);

        if (srcImage.getNDims() >= 3) {
            checkBox25D = new JCheckBox("Process each slice separately", true);
            checkBox25D.setFont(serif12);
        }

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

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(6, 6, 6, 6);

        gbc.gridy = 0;
        gbc.gridx = 0;
        parameterPanel.add(labelNumberIterations, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textNumberIterations, gbc);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelDiffusitivity, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textDiffusitivity, gbc);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelDerivative, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textDerivative, gbc);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelGaussian, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textGaussian, gbc);

        if (srcImage.getNDims() >= 3) {
            gbc.gridy++;
            gbc.gridx = 0;
            parameterPanel.add(checkBox25D, gbc);
        }

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(radioEntireImage, gbc);
        gbc.gridx = 1;
        parameterPanel.add(radioVOIRegion, gbc);

        return parameterPanel;
    } // end buildParameterPanel()

    /**
     * DOCUMENT ME!
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Coherence-Enhancing Diffusion");

        JPanel paramPanel = buildParameterPanel();

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel mainPanel = new JPanel(new GridBagLayout());

        gbc.gridy = 0;
        mainPanel.add(paramPanel, gbc);

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

        tmpStr = textNumberIterations.getText();
        numIterations = Integer.valueOf(tmpStr).intValue();

        tmpStr = textDiffusitivity.getText();
        diffusitivityDenom = Float.valueOf(tmpStr).floatValue();

        tmpStr = textDerivative.getText();
        derivativeScale = Float.valueOf(tmpStr).floatValue();

        tmpStr = textGaussian.getText();
        gaussianScale = Float.valueOf(tmpStr).floatValue();

        if (srcImage.getNDims() >= 3) {
            do25D = checkBox25D.isSelected();
        }

        entireImage = radioEntireImage.isSelected();

        return true;
    } // end setVariables()

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Applies a Coherence Enhancing Diffusion algorithm.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Coherence Enhancing Diffusion algorithm.");
            }

            public String getShortLabel() {
                return new String("CoherenceEnhancingDiffusion");
            }

            public String getLabel() {
                return new String("Coherence Enhancing Diffusion");
            }

            public String getName() {
                return new String("Coherence Enhancing Diffusion");
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
            table.put(new ParameterInt(AlgorithmParameters.NUM_ITERATIONS, 1));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterFloat("gaussian_scale", 2f));
            table.put(new ParameterFloat("derivative_scale", .5f));
            table.put(new ParameterFloat("diffusitivity_denominator", .001f));
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
            } else {
                // algo was done in place
                return srcImage.getImageName();
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
