package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call a specified diffusion algorithm. It should be noted that the algorithms are
 * executed in their own thread.
 */
public class JDialogCoherenceEnhancingDiffusion extends JDialogScriptableBase implements AlgorithmInterface{

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
    private long end;

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
    private long start;

    /** DOCUMENT ME! */
    private JTextField textDerivative, textGaussian;

    /** DOCUMENT ME! */
    private JTextField textNumberIterations, textDiffusitivity;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface = null;

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
        userInterface = ViewUserInterface.getReference();
        init();
    } // end JDialogDiffusion(...)

    

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Record the parameters just used to run this algorithm in a script.
     * 
     * @throws  ParserException  If there is a problem creating/recording the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException{
    try{
        scriptParameters.storeInputImage(srcImage);
        
        
       
        scriptParameters.getParams().put(ParameterFactory.newParameter("derivativeScale",derivativeScale));
        scriptParameters.getParams().put(ParameterFactory.newParameter("diffusitivityDenom",diffusitivityDenom));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do25D",do25D));
        scriptParameters.getParams().put(ParameterFactory.newParameter("end",end));
        scriptParameters.getParams().put(ParameterFactory.newParameter("entireImage",entireImage));
        scriptParameters.getParams().put(ParameterFactory.newParameter("gaussianScale",gaussianScale));
        scriptParameters.getParams().put(ParameterFactory.newParameter("numIterations",numIterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("start",start));
        
    }catch (ParserException pe){
        MipavUtil.displayError("Error encountered saving script params:\n" + pe);
    }  
    }

    
    /**
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
    protected void setGUIFromParams(){
        derivativeScale = scriptParameters.getParams().getFloat("derivativeScale");
        diffusitivityDenom = scriptParameters.getParams().getFloat("diffusitivityDenom");
        do25D = scriptParameters.getParams().getBoolean("do25D");
        end = scriptParameters.getParams().getLong("end");
        entireImage = scriptParameters.getParams().getBoolean("entireImage");
        gaussianScale = scriptParameters.getParams().getFloat("gaussianScale");
        numIterations = scriptParameters.getParams().getInt("numIterations");
        start = scriptParameters.getParams().getLong("start");
    }
    
    /**
     * Used to perform actions after the execution of the algorithm is completed (e.g., put the result image in the image table).
     * Defaults to no action, override to actually have it do something.
     */
    protected void doPostAlgorithmActions() {
            AlgorithmParameters.storeImageInRunner(getResultImage());
     }
    
    
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
        ViewJFrameImage imageFrame = null;

        end = System.currentTimeMillis();
        Preferences.debug("CoherenceEnhancingDiffusion time: " + (end - start));

        if (((algorithm instanceof AlgorithmCoherenceEnhancingDiffusion) &&
                 (coherenceEnhancingDiffusionAlgo.isCompleted() == true)) && (resultImage != null)) {

            updateFileInfo(srcImage, resultImage);
            resultImage.clearMask();

            try {
                imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            insertScriptLine();

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
     *
     * @apram  do25D
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
     * Once all the necessary variables are set, call the mean algorithm based on what type of image this is and whether
     * or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        start = System.currentTimeMillis();

        float[] sigmas = null;
        String name;

        name = makeImageName(srcImage.getImageName(), "_ce");

        try {

            if (srcImage.isColorImage()) {
                resultImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), name, userInterface);
            } else {
                resultImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), name, userInterface);
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

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (coherenceEnhancingDiffusionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!userInterface.isAppFrameVisible()) {
                    coherenceEnhancingDiffusionAlgo.setProgressBarVisible(false);
                }

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
        setResizable(false);

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

} // end class JDialogCohEnhDiffusion
