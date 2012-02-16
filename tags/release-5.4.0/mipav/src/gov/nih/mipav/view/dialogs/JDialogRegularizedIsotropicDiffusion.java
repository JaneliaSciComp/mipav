package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmFFT;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.WidgetFactory;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call a specified diffusion algorithm. It should be noted that the algorithms are
 * executed in their own thread.
 */
public class JDialogRegularizedIsotropicDiffusion extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 781611739174870058L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox checkBox25D;
    private JCheckBox useOCLCheckbox;

    /** DOCUMENT ME! */
    private float contrast = 0.15f;

    /** DOCUMENT ME! */
    private boolean do25D = true;
    private boolean doOCL = false;

    /** DOCUMENT ME! */
    private int numIterations = 1;

    /** DOCUMENT ME! */
    private AlgorithmRegularizedIsotropicDiffusion regIsoDiffusionAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private float stdDev = 1.0f;

    /** DOCUMENT ME! */
    private JTextField textNumberIterations, textGaussian, textContrast;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegularizedIsotropicDiffusion() { }


    /**
     * Creates a new JDialogRegularizedIsotropicDiffusion object.
     *
     * @param  frame  DOCUMENT ME!
     * @param  im     DOCUMENT ME!
     */
    public JDialogRegularizedIsotropicDiffusion(Frame frame, ModelImage im) {
        super(frame, false);

        srcImage = im;
        init();
    } // end JDialogDiffusion(...)

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
            MipavUtil.showHelp("10088");
        } // end if()-else
    } // end actionPerformed(...)

    /**
     * DOCUMENT ME!
     *
     * @param  algorithm  DOCUMENT ME!
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (((algorithm instanceof AlgorithmRegularizedIsotropicDiffusion) &&
                 (regIsoDiffusionAlgo.isCompleted() == true)) && (resultImage != null)) {

            updateFileInfo(srcImage, resultImage);
            resultImage.clearMask();

            try {
                new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            insertScriptLine();

            regIsoDiffusionAlgo.finalize();
            regIsoDiffusionAlgo = null;
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
     * Accessor that sets the contrast.
     *
     * @param  contrast  DOCUMENT ME!
     */
    public void setContrast(float contrast) {
        this.contrast = contrast;
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
     * Accessor that sets the number of iterations.
     *
     * @param  numIterations  DOCUMENT ME!
     */
    public void setNumIterations(int numIterations) {
        this.numIterations = numIterations;
    }

    /**
     * Accessor that sets the standard deviastion.
     *
     * @param  stdDev  DOCUMENT ME!
     */
    public void setStdDev(float stdDev) {
        this.stdDev = stdDev;
    }


    /**
     * Once all the necessary variables are set, call the mean algorithm based on what type of image this is and whether
     * or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name;

        name = makeImageName(srcImage.getImageName(), "_rid");
        
        try {

            if (srcImage.isColorImage()) {
                resultImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), name);
            } else {
                resultImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), name);
            }
            /*
            if ( doOCL )
            {
            	OpenCLAlgorithmRegularizedIsotropicDiffusion regIsoDiffusionAlgorithm = new OpenCLAlgorithmRegularizedIsotropicDiffusion(resultImage, srcImage, numIterations,
                        stdDev, contrast, do25D);
            	regIsoDiffusionAlgorithm.addListener(this);
            	regIsoDiffusionAlgorithm.run();
            	return;
            }*/

            regIsoDiffusionAlgo = new AlgorithmRegularizedIsotropicDiffusion(resultImage, srcImage, numIterations,
                                                                             stdDev, contrast, do25D);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            regIsoDiffusionAlgo.addListener(this);

            createProgressBar(srcImage.getImageName(), regIsoDiffusionAlgo);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (regIsoDiffusionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                regIsoDiffusionAlgo.run();
            } // end if (isRunInSeparateThread())
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("JDialogRegIsoDiffusion: unable to allocate enough memory");

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

        setDo25D(scriptParameters.doProcess3DAs25D());
        setNumIterations(scriptParameters.getNumIterations());
        setStdDev(scriptParameters.getParams().getFloat(AlgorithmParameters.SIGMAS));
        setContrast(scriptParameters.getParams().getFloat("diffusion_contrast"));
    }


    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);
        scriptParameters.storeImageInRecorder(getResultImage());

        scriptParameters.storeProcess3DAs25D(do25D);
        scriptParameters.storeNumIterations(numIterations);
        scriptParameters.getParams().put(ParameterFactory.newParameter(AlgorithmParameters.SIGMAS, stdDev));
        scriptParameters.getParams().put(ParameterFactory.newParameter("diffusion_contrast", contrast));
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

        JLabel labelNumberIterations = new JLabel("Number of Iterations");
        labelNumberIterations.setFont(serif12);
        textNumberIterations = new JTextField();
        textNumberIterations.setText(String.valueOf(numIterations));
        textNumberIterations.setColumns(5);
        textNumberIterations.setMaximumSize(textNumberIterations.getPreferredSize());
        textNumberIterations.setHorizontalAlignment(JTextField.RIGHT);
        textNumberIterations.setFont(serif12);

        JLabel labelGaussian = new JLabel("Gaussian standard deviation");
        labelGaussian.setFont(serif12);
        textGaussian = new JTextField();
        textGaussian.setText(String.valueOf(stdDev));
        textGaussian.setColumns(5);
        textGaussian.setMaximumSize(textGaussian.getPreferredSize());
        textGaussian.setHorizontalAlignment(JTextField.RIGHT);
        textGaussian.setFont(serif12);

        JLabel labelContrast = new JLabel("Diffusion contrast parameter");
        labelContrast.setFont(serif12);
        textContrast = new JTextField();
        textContrast.setText(String.valueOf(contrast));
        textContrast.setColumns(5);
        textContrast.setMaximumSize(textContrast.getPreferredSize());
        textContrast.setHorizontalAlignment(JTextField.RIGHT);
        textContrast.setFont(serif12);

        if (srcImage.getNDims() >= 3) {
            checkBox25D = new JCheckBox("Process each slice separately", true);
            checkBox25D.setFont(serif12);
        }
        useOCLCheckbox = WidgetFactory.buildCheckBox("Use OpenCL", false, this);
        useOCLCheckbox.setFont(serif12);
        useOCLCheckbox.setForeground(Color.black);
    	useOCLCheckbox.setEnabled(Preferences.isGpuCompEnabled() && OpenCLAlgorithmFFT.isOCLAvailable());
    	if ( !useOCLCheckbox.isEnabled() && OpenCLAlgorithmFFT.isOCLAvailable() )
    	{
    		useOCLCheckbox.setToolTipText( "see Help->Mipav Options->Other to enable GPU computing");
    	}

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
        parameterPanel.add(labelGaussian, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textGaussian, gbc);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelContrast, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textContrast, gbc);

        if (srcImage.getNDims() >= 3) {
            gbc.gridy++;
            gbc.gridx = 0;
            parameterPanel.add(checkBox25D, gbc);
        }
        gbc.gridy++;
        gbc.gridx = 0;
        //parameterPanel.add(useOCLCheckbox, gbc);


        return parameterPanel;
    } // end buildParameterPanel()

    /**
     * DOCUMENT ME!
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Regularized Isotropic Diffusion");

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

        tmpStr = textGaussian.getText();
        stdDev = Float.valueOf(tmpStr).floatValue();

        tmpStr = textContrast.getText();
        contrast = Float.valueOf(tmpStr).floatValue();

        if (srcImage.getNDims() >= 3) {
            do25D = checkBox25D.isSelected();
        }

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
                return new String("Applies a regularized isotropic diffusion filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a regularized isotropic diffusion filter.");
            }

            public String getShortLabel() {
                return new String("RegularizedIsotropicDiffusion");
            }

            public String getLabel() {
                return new String("Regularized Isotropic Diffusion");
            }

            public String getName() {
                return new String("Regularized Isotropic Diffusion");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterInt(AlgorithmParameters.NUM_ITERATIONS, 10));
            table.put(new ParameterFloat(AlgorithmParameters.SIGMAS, 1.0f));
            table.put(new ParameterFloat("diffusion_contrast", 0.15f));
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

} // end class JDialogRegIsoDiffusion
