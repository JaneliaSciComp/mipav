package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;

import java.awt.event.*;
import java.awt.*;

import javax.swing.*;

/**
 *   Dialog to get user input, then call a specified diffusion algorithm.
 *   It should be noted that the algorithms are executed in their own
 *   thread.
 */
public class JDialogCoherenceEnhancingDiffusion
    extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    private AlgorithmCoherenceEnhancingDiffusion coherenceEnhancingDiffusionAlgo = null;

    private ModelImage srcImage;
    private ModelImage resultImage;
    private ViewUserInterface userInterface = null;
    private int numIterations = 1;
    private float diffusitivityDenom = 0.001f;
    private float derivativeScale = 0.5f;
    private float gaussianScale = 2.0f;
    private boolean do25D = true;
    private boolean entireImage = true;

    private JTextField textNumberIterations, textDiffusitivity;
    private JTextField textDerivative, textGaussian;
    private JCheckBox checkBox25D;
    private JRadioButton radioEntireImage, radioVOIRegion;

    private long start;
    private long end;

    /**
     * Initialize the dialog.
     * @param frame  the parent frame
     * @param im     the source image
     */
    public JDialogCoherenceEnhancingDiffusion(Frame frame, ModelImage im) {
        super(frame, false);

        srcImage = im;
        userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    } // end JDialogDiffusion(...)

    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param UI   The user interface, needed to create the image frame.
     *	@param im	Source image.
     */
    public JDialogCoherenceEnhancingDiffusion(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;
        srcImage = im;
        resultImage = im; // will be overridden if the algorithm is actually run..
        parentFrame = im.getParentFrame();
    }

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogCoherenceEnhancingDiffusion() {}

    /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        }
        catch (Exception e) {
            throw new IllegalArgumentException();
        }
        ModelImage im = parser.getImage(srcImageKey);

        srcImage = im;
        userInterface = srcImage.getUserInterface();
        parentFrame = srcImage.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        }
        catch (Exception e) {
            throw new IllegalArgumentException();
        }

        try {
            setNumIterations(parser.getNextInteger());
            setDiffusitivityDenom(parser.getNextFloat());
            setDerivativeScale(parser.getNextFloat());
            setGaussianScale(parser.getNextFloat());
            setDo25D(parser.getNextBoolean());
            entireImage = parser.getNextBoolean();
        }
        catch (Exception e) {
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
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     * @param algo the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {
        if (algo.isCompleted()) {
            if (userInterface.isScriptRecording()) {
                //check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(srcImage.getImageName()) == null) {
                    if (userInterface.getScriptDialog().getActiveImgTableVar(srcImage.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(srcImage.getImageName());
                    }
                }

                userInterface.getScriptDialog().append(
                    "CoherenceEnhancingDiffusion "
                    + userInterface.getScriptDialog().getVar(srcImage.getImageName()) + " ");
                //if (displayLoc == NEW) {
                userInterface.getScriptDialog().putVar(resultImage.getImageName());
                userInterface.getScriptDialog().append(
                    userInterface.getScriptDialog().getVar(resultImage.getImageName()) + " " + numIterations + " "
                    + diffusitivityDenom + " " + derivativeScale + " " + gaussianScale + " " + do25D + " "
                    + entireImage + "\n");
            }
        }
    }

    private void init() {
        setForeground(Color.black);
        setTitle("Coherence-Enhancing Diffusion");
        JPanel paramPanel = buildParameterPanel();

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.fill = gbc.HORIZONTAL;

        JPanel mainPanel = new JPanel(new GridBagLayout());

        gbc.gridy = 0;
        mainPanel.add(paramPanel, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);

    } // end init()

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

        gbc.anchor = gbc.WEST;
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
     *  Accessor that sets the number of iterations
     *  @param numIterations
     */
    public void setNumIterations(int numIterations) {
        this.numIterations = numIterations;
    }

    /**
     *  Accessor that sets the diffusitivityDenom
     *  @param diffusitivityDenom
     */
    public void setDiffusitivityDenom(float diffusitivityDenom) {
        this.diffusitivityDenom = diffusitivityDenom;
    }

    /**
     *  Accessor that sets the derivativeScale
     *  @param derivativeScale
     */
    public void setDerivativeScale(float derivativeScale) {
        this.derivativeScale = derivativeScale;
    }

    /**
     *  Accessor that sets the gaussian scale
     *  @param gaussianScale
     */
    public void setGaussianScale(float gaussianScale) {
        this.gaussianScale = gaussianScale;
    }

    /**
     *  Accessor that sets if slice by slice processing occurs
     *  @apram do25D
     */
    public void setDo25D(boolean do25D) {
        this.do25D = do25D;
    }

    /**
     *  Accessor that returns the image.
     *  @return          The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

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
     *  Closes dialog box when the OK button is pressed and calls the algorithm.
     *  @param event       Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        }
        else if (command.equals("Cancel")) {
            dispose();
        }
        else if (command.equals("Help")) {
            MipavUtil.showHelp("10086");
        } // end if()-else
    } // end actionPerformed(...)

    /**
     *	Once all the necessary variables are set, call the mean
     *	algorithm based on what type of image this is and whether or not there
     *	is a separate destination image.
     */
    private void callAlgorithm() {
        start = System.currentTimeMillis();

        float[] sigmas = null;
        String name;

        name = makeImageName(srcImage.getImageName(), "_ce");

        try {
            if (srcImage.isColorImage()) {
                resultImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), name, userInterface);
            }
            else {
                resultImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), name, userInterface);
            }

            coherenceEnhancingDiffusionAlgo = new AlgorithmCoherenceEnhancingDiffusion(resultImage, srcImage,
                numIterations, diffusitivityDenom, derivativeScale, gaussianScale, do25D, entireImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            coherenceEnhancingDiffusionAlgo.addListener(this);

            if (runInSeparateThread) {
                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (coherenceEnhancingDiffusionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            }
            else {
                coherenceEnhancingDiffusionAlgo.setActiveImage(isActiveImage);
                if (!userInterface.isAppFrameVisible()) {
                    coherenceEnhancingDiffusionAlgo.setProgressBarVisible(false);
                }
                coherenceEnhancingDiffusionAlgo.run();
            } // end if (runInSeparateThread)

        }
        catch (OutOfMemoryError x) {
            MipavUtil.displayError("JDialogCohEnhDiffusion: unable to allocate enough memory");
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }
            return;
        } // end try()=catch()

        dispose();
    } // end callAlgorithm()

    public void algorithmPerformed(AlgorithmBase algorithm) {
        ViewJFrameImage imageFrame = null;

        end = System.currentTimeMillis();
        Preferences.debug("CoherenceEnhancingDiffusion time: " + (end - start));

        if ( (algorithm instanceof AlgorithmCoherenceEnhancingDiffusion
              && coherenceEnhancingDiffusionAlgo.isCompleted() == true)
            && resultImage != null) {

            updateFileInfo(srcImage, resultImage);
            resultImage.clearMask();
            try {
                imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            }
            catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            insertScriptLine(algorithm);

            coherenceEnhancingDiffusionAlgo.finalize();
            coherenceEnhancingDiffusionAlgo = null;
        }
    } // end algorithmPerformed(...)

} // end class JDialogCohEnhDiffusion