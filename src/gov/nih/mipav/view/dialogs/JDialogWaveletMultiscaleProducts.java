package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogWaveletMultiscaleProducts extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
    public static final int MINIMUM_PHASE = 1;
    
    public static final int MID_PHASE = 2;
    
    public static final int MAXIMUM_PHASE = 3;
    
    public static final int MAD = 0; // Mean absolute deviation
    
    public static final int STD = 1; // Classical numerical std estimate
    
    public static final int SOFT_THRESHOLDING = 0;
    
    public static final int HARD_THRESHOLDING = 1;

    private int filterLength;
    
    private JRadioButton redundantButton;
    private JRadioButton nonredundantButton;
    private boolean redundant = true;

    /** DOCUMENT ME! */
    private JTextField textFilterLength;

    

    /** DOCUMENT ME! */
    private boolean doWaveletImages;

    
    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private AlgorithmRiceWaveletTools waveletAlgo;

    /** DOCUMENT ME! */
    private JCheckBox waveletCheckBox;
    
    private ModelImage destImage = null;

    /** DOCUMENT ME! */
    private ModelImage[] waveletImage = null;
    
    // numberOfLevels == Integer.MAX_VALUE means that the maximum possible number of levels
    // is calculated and used.
    private int numberOfLevels = Integer.MAX_VALUE;
    
    private JRadioButton maximumLevelsButton;
    
    private JRadioButton userLevelsButton;
    
    private JRadioButton minimumButton;
    
    private JRadioButton midButton;
    
    private JRadioButton maximumButton;
    
    private int filterType = MINIMUM_PHASE;
    
    private JTextField textLevels;
    
    private int xDim;
    
    private int yDim;
    
    private int minimumLevel;
    
    private int maximumLevel;
    
    private JTextField textMinimum;
    
    private JTextField textMaximum;
    
    private boolean doDenoise = false;
    // actual_threshold used if value is other than zero
    private double actualThreshold = 0.0;
    private int varianceEstimator = MAD;
    // The threshold thld is computed as thld = c*MAD(noise_estimate);
    // Defaults are 3.6 for undecimated or redundant based denoising
    // and 3.0 if not decimated or redundant
    private double thresholdMultiplier = 3.6;
    // Defaults are SOFT_THRESHOLDING if not redundant and HARD_THRESHOLDING if redundant
    private int thresholdingType = HARD_THRESHOLDING;
    // Default is don't threshold low pass components
    private boolean thresholdLowPass = false;
    
    private JCheckBox denoiseCheckBox;
    private JCheckBox thresholdLowPassCheckBox;
    private JLabel thresholdMultiplierLabel;
    private JTextField thresholdMultiplierText;
    private JLabel varianceEstimatorLabel;
    private ButtonGroup varianceGroup;
    private JRadioButton MADButton;
    private JRadioButton STDButton;
    private JLabel thresholdingTypeLabel;
    private ButtonGroup thresholdingTypeGroup;
    private JRadioButton softButton;
    private JRadioButton hardButton;
    private JLabel actualThresholdLabel;
    private JTextField actualThresholdText;
    private JCheckBox BayesCheckBox;
	private boolean doBayesShrinkThresholdComputation = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogWaveletMultiscaleProducts() { }

    /**
     * Creates new dialog for entering parameters for wavelet multiscale products.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogWaveletMultiscaleProducts(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        xDim = image.getExtents()[0];
        if ((xDim % 2) == 1) {
            MipavUtil.displayError("1 level of decomposition requires an even xDim");
            return;
        }
        yDim = image.getExtents()[1];
        if ((yDim % 2) == 1) {
            MipavUtil.displayError("1 level of decomposition requires an even yDim");
            return;
        }
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
        } else if ((source == maximumLevelsButton) || (source == userLevelsButton)) {
            if (maximumLevelsButton.isSelected()) {
                textLevels.setEnabled(false);
            }
            else {
                textLevels.setEnabled(true);
            }
        } else if (source == denoiseCheckBox) {
            doDenoise = denoiseCheckBox.isSelected();
            thresholdLowPassCheckBox.setEnabled(doDenoise);
            BayesCheckBox.setEnabled(doDenoise);
            if (BayesCheckBox.isSelected()) {
            	thresholdMultiplierLabel.setEnabled(false);
                thresholdMultiplierText.setEnabled(false);
                actualThresholdLabel.setEnabled(false);
                actualThresholdText.setEnabled(false);
            }
            else {
	            thresholdMultiplierLabel.setEnabled(doDenoise);
	            thresholdMultiplierText.setEnabled(doDenoise);
	            actualThresholdLabel.setEnabled(doDenoise);
	            actualThresholdText.setEnabled(doDenoise);
            }
            MADButton.setEnabled(doDenoise);
            STDButton.setEnabled(doDenoise);
            softButton.setEnabled(doDenoise);
            hardButton.setEnabled(doDenoise);
        } else if (source == BayesCheckBox) {
        	if (BayesCheckBox.isSelected()) {
            	thresholdMultiplierLabel.setEnabled(false);
                thresholdMultiplierText.setEnabled(false);
                actualThresholdLabel.setEnabled(false);
                actualThresholdText.setEnabled(false);
            }
            else {
	            thresholdMultiplierLabel.setEnabled(true);
	            thresholdMultiplierText.setEnabled(true);
	            actualThresholdLabel.setEnabled(true);
	            actualThresholdText.setEnabled(true);
            }
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
        int i;
        int j = 0;

        if (algorithm instanceof AlgorithmRiceWaveletTools) {
            image.clearMask();

            if (waveletAlgo.isCompleted()) {
                
                if (doDenoise && (destImage != null)) {
                	updateFileInfo(image, destImage);
                	
                	try {
                        new ViewJFrameImage(destImage, null, new Dimension(610, 220));
                        j++;
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: Unable to open denoised image frame");
                    }
                }
                else if (doDenoise) {
                	MipavUtil.displayError("destImage is null");
                }
                
                waveletImage = waveletAlgo.getWaveletImages();

                if (waveletImage != null) {
                    for (i = 0; i < waveletImage.length; i++) {
                        
                        if (waveletImage[i] != null) {

                            // waveletImage is same size as original image
                            updateFileInfo(image, waveletImage[i]);
    
                            try {
                                new ViewJFrameImage(waveletImage[i], null, new Dimension(610, (220 + (i +j)* 20)));
                            } catch (OutOfMemoryError error) {
                                MipavUtil.displayError("Out of memory: Unable to open wavelet image frame");
                            }
                        } // if (waveletImage[i] != null)
                        else {
                            MipavUtil.displayError("waveletImage[" + i + "] is null");
                        }
                    } // for (i = 0; i < waveletImage.length; i++)
                } else if (doWaveletImages || (maximumLevel > minimumLevel)){
                    MipavUtil.displayError("waveletImage array is null");
                }
          
            }
        }
        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        waveletAlgo.finalize();
        waveletAlgo = null;
        dispose();
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        // Object source = event.getSource();
        // float tempNum;

    }

    /**
     * Accessor that sets the filter length.
     *
     * @param  filterLength  DOCUMENT ME!
     */
    public void setFilterLength(int filterLength) {
        this.filterLength = filterLength;
    }
    
    public void setNumberOfLevels(int numberOfLevels) {
        this.numberOfLevels = numberOfLevels;
    }

    /**
     * Accessor that sets whether or not the wavelet images are displayed.
     *
     * @param  doWaveletImage  DOCUMENT ME!
     */
    public void setDoWaveletImage(boolean doWaveletImages) {
        this.doWaveletImages = doWaveletImages;
    }
    
    public void setMinimumLevel(int minimumLevel) {
        this.minimumLevel = minimumLevel;
    }
    
    public void setMaximumLevel(int maximumLevel) {
        this.maximumLevel = maximumLevel;
    }

    /**
     * Once all the necessary variables are set, call the UnsharpMark algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int i;

            int[] destExtents = new int[image.getExtents().length];
            for (i = 0; i < image.getExtents().length; i++) {
                destExtents[i] = image.getExtents()[i];    
            }
            
            if (doDenoise) {
            	destImage = new ModelImage(image.getDataType(), destExtents, image.getImageName() + "_denoise");
            }

            try {
                // Make algorithm
                waveletAlgo = new AlgorithmRiceWaveletTools(destImage, image, filterLength, redundant,
                                  numberOfLevels, doWaveletImages, minimumLevel, maximumLevel,
                                  filterType, doDenoise,  actualThreshold, 
                                  varianceEstimator, thresholdMultiplier, thresholdingType, thresholdLowPass,
                                  doBayesShrinkThresholdComputation);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                waveletAlgo.addListener(this);

                createProgressBar(image.getImageName(), waveletAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (waveletAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    waveletAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog WaveletThreshold: unable to allocate enough memory");
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
        parentFrame = image.getParentFrame();

        filterLength = scriptParameters.getParams().getInt("filter_length");
        redundant = scriptParameters.getParams().getBoolean("redun");
        numberOfLevels = scriptParameters.getParams().getInt("number_of_levels");
        doWaveletImages = scriptParameters.getParams().getBoolean("do_show_wavelet_images");
        minimumLevel = scriptParameters.getParams().getInt("minimum_level");
        maximumLevel = scriptParameters.getParams().getInt("maximum_level");
        filterType = scriptParameters.getParams().getInt("filter_type");
        doDenoise = scriptParameters.getParams().getBoolean("do_denoise");
        actualThreshold = scriptParameters.getParams().getDouble("actual_threshold");
        varianceEstimator = scriptParameters.getParams().getInt("variance_estimator");
        thresholdMultiplier = scriptParameters.getParams().getDouble("threshold_multiplier");
        thresholdingType = scriptParameters.getParams().getInt("thresholding_type");
        thresholdLowPass = scriptParameters.getParams().getBoolean("threshold_low_pass");
        doBayesShrinkThresholdComputation = scriptParameters.getParams().getBoolean("do_Bayes");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.getParams().put(ParameterFactory.newParameter("filter_length", filterLength));
        scriptParameters.getParams().put(ParameterFactory.newParameter("redun", redundant));
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_levels", numberOfLevels));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_show_wavelet_images", doWaveletImages));
        scriptParameters.getParams().put(ParameterFactory.newParameter("minimum_level", minimumLevel));
        scriptParameters.getParams().put(ParameterFactory.newParameter("maximum_level", maximumLevel));
        scriptParameters.getParams().put(ParameterFactory.newParameter("filter_type", filterType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_denoise", doDenoise));
        scriptParameters.getParams().put(ParameterFactory.newParameter("actual_threshold", actualThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("varianceEstimator", varianceEstimator));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_multiplier", thresholdMultiplier));
        scriptParameters.getParams().put(ParameterFactory.newParameter("thresholding_type", thresholdingType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_low_pass", thresholdLowPass));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_Bayes", doBayesShrinkThresholdComputation));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Wavelet Multiscale Products");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        JLabel coeffLabel = new JLabel("Filter length (must be even)");
        coeffLabel.setForeground(Color.black);
        coeffLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(coeffLabel, gbc);

        textFilterLength = new JTextField(10);
        textFilterLength.setFont(serif12);
        textFilterLength.setText("4");
        gbc.gridx = 1;
        paramPanel.add(textFilterLength, gbc);
        
        ButtonGroup redundantGroup = new ButtonGroup();
        redundantButton = new JRadioButton("Redundant with no sub-sampling",true);
        redundantButton.setFont(serif12);
        redundantGroup.add(redundantButton);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(redundantButton, gbc);
        
        nonredundantButton = new JRadioButton("Nonredundant with sub-sampling",false);
        nonredundantButton.setFont(serif12);
        redundantGroup.add(nonredundantButton);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(nonredundantButton, gbc);
        
        JLabel levelsLabel = new JLabel("Number of levels:");
        levelsLabel.setForeground(Color.black);
        levelsLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(levelsLabel, gbc);
        
        ButtonGroup levelsGroup = new ButtonGroup();  
        userLevelsButton = new JRadioButton("User specified", true);
        userLevelsButton.setFont(serif12);
        userLevelsButton.addActionListener(this);
        levelsGroup.add(userLevelsButton);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(userLevelsButton, gbc);
        
        textLevels = new JTextField(10);
        textLevels.setFont(serif12);
        textLevels.setText("2");
        textLevels.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(textLevels, gbc);
        maximumLevelsButton = new JRadioButton("Maximum possible", false);
        maximumLevelsButton.setFont(serif12);
        maximumLevelsButton.addActionListener(this);
        levelsGroup.add(maximumLevelsButton);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(maximumLevelsButton, gbc);
        
        JLabel minimumLabel = new JLabel("Minimum level for multiplication (>=1):");
        minimumLabel.setForeground(Color.black);
        minimumLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(minimumLabel, gbc);
        
        textMinimum = new JTextField(10);
        textMinimum.setFont(serif12);
        textMinimum.setText("1");
        gbc.gridx = 1;
        paramPanel.add(textMinimum, gbc);
       
        JLabel maximumLabel = new JLabel("Maximum level for multiplication (<= Number of levels):");
        maximumLabel.setForeground(Color.black);
        maximumLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(maximumLabel, gbc);
        
        textMaximum = new JTextField(10);
        textMaximum.setFont(serif12);
        textMaximum.setText("2");
        gbc.gridx = 1;
        paramPanel.add(textMaximum, gbc);  
        
        ButtonGroup phaseGroup = new ButtonGroup();  
        minimumButton = new JRadioButton("Minimum phase", true);
        minimumButton.setFont(serif12);
        phaseGroup.add(minimumButton);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(minimumButton, gbc);
        
        midButton = new JRadioButton("Mid phase", false);
        midButton.setFont(serif12);
        phaseGroup.add(midButton);
        gbc.gridx = 0;
        gbc.gridy = 9;
        paramPanel.add(midButton, gbc);
        
        maximumButton = new JRadioButton("Maximum phase", false);
        maximumButton.setFont(serif12);
        phaseGroup.add(maximumButton);
        gbc.gridx = 0;
        gbc.gridy = 10;
        paramPanel.add(maximumButton, gbc);

        waveletCheckBox = new JCheckBox("Display individual level wavelet images");
        waveletCheckBox.setFont(serif12);
        waveletCheckBox.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 2;
        paramPanel.add(waveletCheckBox, gbc);
        
        denoiseCheckBox = new JCheckBox("Create denoised image");
        denoiseCheckBox.setFont(serif12);
        denoiseCheckBox.setSelected(true);
        denoiseCheckBox.addActionListener(this);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 2;
        paramPanel.add(denoiseCheckBox, gbc);
        
        thresholdLowPassCheckBox = new JCheckBox("Threshold low pass component");
        thresholdLowPassCheckBox.setFont(serif12);
        thresholdLowPassCheckBox.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 2;
        paramPanel.add(thresholdLowPassCheckBox, gbc);
        
        thresholdMultiplierLabel = new JLabel("Threshold multiplier");
        thresholdMultiplierLabel.setForeground(Color.black);
        thresholdMultiplierLabel.setFont(serif12);
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(thresholdMultiplierLabel, gbc);
        
        thresholdMultiplierText = new JTextField(10);
        thresholdMultiplierText.setFont(serif12);
        thresholdMultiplierText.setText("3.6");
        gbc.gridx = 1;
        paramPanel.add(thresholdMultiplierText, gbc);
        
        varianceEstimatorLabel = new JLabel("Variance estimator type:");
        varianceEstimatorLabel.setForeground(Color.black);
        varianceEstimatorLabel.setFont(serif12);
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(varianceEstimatorLabel, gbc);
        
        varianceGroup = new ButtonGroup();
        MADButton = new JRadioButton("Mean absolute deviation",true);
        MADButton.setFont(serif12);
        varianceGroup.add(MADButton);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(MADButton, gbc);
        
        STDButton = new JRadioButton("Numerical standard deviation", false);
        STDButton.setFont(serif12);
        varianceGroup.add(STDButton);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(STDButton, gbc);
        
        thresholdingTypeLabel = new JLabel("Thresholding type:");
        thresholdingTypeLabel.setForeground(Color.black);
        thresholdingTypeLabel.setFont(serif12);
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(thresholdingTypeLabel, gbc);
        
        thresholdingTypeGroup = new ButtonGroup();
        softButton = new JRadioButton("Soft thresholding", false);
        softButton.setFont(serif12);
        thresholdingTypeGroup.add(softButton);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(softButton, gbc);
        
        hardButton = new JRadioButton("Hard thresholding", true);
        hardButton.setFont(serif12);
        thresholdingTypeGroup.add(softButton);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(hardButton, gbc);
        
        BayesCheckBox = new JCheckBox("Bayes shrink threshold computation");
        BayesCheckBox.setFont(serif12);
        BayesCheckBox.setForeground(Color.black);
        BayesCheckBox.setSelected(false);
        BayesCheckBox.addActionListener(this);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(BayesCheckBox, gbc);
        
        actualThresholdLabel = new JLabel("Actual threshold (if > 0.0)");
        actualThresholdLabel.setForeground(Color.black);
        actualThresholdLabel.setFont(serif12);
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(actualThresholdLabel, gbc);
        
        actualThresholdText = new JTextField(10);
        actualThresholdText.setFont(serif12);
        actualThresholdText.setText("0.0");
        gbc.gridx = 1;
        paramPanel.add(actualThresholdText, gbc);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        mainPanel.add(paramPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        //setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        System.gc();

        String tmpStr;
        int divisor;
        int i, j, k;

        tmpStr = textFilterLength.getText();
        
        if (testParameter(tmpStr, 2, 100)) {
            filterLength = Integer.valueOf(tmpStr).intValue();
        }
        else {
            textFilterLength.requestFocus();
            textFilterLength.selectAll();
            
            return false;
        }
        
        if ((filterLength % 2) == 1) {
            MipavUtil.displayError("Filter length must be even");
            return false;
        }
        
        redundant = redundantButton.isSelected();
        
        if (maximumLevelsButton.isSelected()) {
            i = xDim;
            j = 0;
            while ((i % 2) == 0) {
                i = (i >> 1);
                j++;
            }
            k = yDim;
            i = 0;
            while((k % 2) == 0) {
                k = (k >> 1);
                i++;
            }
            
            numberOfLevels = Math.min(i, j);
            Preferences.debug("The maximum possible number of levels = " + numberOfLevels + "\n", Preferences.DEBUG_FILEIO);
        }
        else {
            tmpStr = textLevels.getText();
            if (testParameter(tmpStr, 1, 20)) {
                numberOfLevels = Integer.valueOf(tmpStr).intValue();
            } else {
                textLevels.requestFocus();
                textLevels.selectAll();
                
                return false;
            }
            divisor = 1;
            for (i = 1; i <= numberOfLevels; i++) {
                divisor *= 2;
            }
            if ((xDim % divisor) != 0) {
                MipavUtil.displayError("Error!  xDim mod " + divisor + " does not equal 0");
                textLevels.requestFocus();
                textLevels.selectAll();
                
                return false;
            }
            
            if ((yDim % divisor) != 0) {
                MipavUtil.displayError("Error!  yDim mod " + divisor + " does not equal 0");
                textLevels.requestFocus();
                textLevels.selectAll();
                
                return false;
            }
        }
        
        tmpStr = textMinimum.getText();
        
        if (testParameter(tmpStr, 1, numberOfLevels)) {
            minimumLevel = Integer.valueOf(tmpStr).intValue();
        }
        else {
            textMinimum.requestFocus();
            textMinimum.selectAll();
            
            return false;
        }
        
        tmpStr = textMaximum.getText();
        
        if (testParameter(tmpStr, minimumLevel, numberOfLevels)) {
            maximumLevel = Integer.valueOf(tmpStr).intValue();
        }
        else {
            textMaximum.requestFocus();
            textMaximum.selectAll();
            
            return false;
        }
        
        if (minimumButton.isSelected()) {
        	filterType = MINIMUM_PHASE;
        }
        else if (midButton.isSelected()) {
        	filterType = MID_PHASE;
        }
        else {
        	filterType = MAXIMUM_PHASE;
        }

        if (waveletCheckBox.isSelected()) {
            doWaveletImages = true;
        } else {
            doWaveletImages = false;
        }
        
        doDenoise = denoiseCheckBox.isSelected();
        
        if (doDenoise) {
        	doBayesShrinkThresholdComputation = BayesCheckBox.isSelected();
        	 
            thresholdLowPass = thresholdLowPassCheckBox.isSelected();
            
            if (!doBayesShrinkThresholdComputation) {
            tmpStr = thresholdMultiplierText.getText();
            if (testParameter(tmpStr, Double.MIN_VALUE, Double.MAX_VALUE)) {
                thresholdMultiplier = Double.valueOf(tmpStr).doubleValue();
            }
            else {
                thresholdMultiplierText.requestFocus();
                thresholdMultiplierText.selectAll();
                
                return false;
            }
            } // if (!doBayesShrinkThresholdComputation) 
            
            if (MADButton.isSelected()) {
            	varianceEstimator = MAD;
            }
            else {
            	varianceEstimator = STD;
            }
            
            if (softButton.isSelected()) {
            	thresholdingType = SOFT_THRESHOLDING;
            }
            else {
            	thresholdingType = HARD_THRESHOLDING;
            }
            
            if (!doBayesShrinkThresholdComputation) {
	            tmpStr = actualThresholdText.getText();
	            if (testParameter(tmpStr, 0.0, Double.MAX_VALUE)) {
	                actualThreshold = Double.valueOf(tmpStr).doubleValue();
	            }
	            else {
	                actualThresholdText.requestFocus();
	                actualThresholdText.selectAll();
	                
	                return false;
	            }
            } // if (!doBayesShrinkThresholdComputation) 
        } // if (doDenoise)

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
                return new String("Algorithms.Filters (wavelet)");
            }

            public String getDescription() {
                return new String("Applies a wavelet threshold.");
            }

            public String getDescriptionLong() {
                return new String("Applies a wavelet threshold.");
            }

            public String getShortLabel() {
                return new String("WaveletThreshold");
            }

            public String getLabel() {
                return new String("Wavelet Threshold");
            }

            public String getName() {
                return new String("Wavelet Threshold");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterInt("filter_Length", 4));
            table.put(new ParameterBoolean("redun", true));
            table.put(new ParameterInt("number_of_levels", Integer.MAX_VALUE));
            table.put(new ParameterBoolean("do_show_wavelet_images", false));
            table.put(new ParameterInt("minimum_level", 1));
            table.put(new ParameterInt("maximum_level", 2));
            table.put(new ParameterInt("filter_type", MINIMUM_PHASE));
            table.put(new ParameterBoolean("do_denoise", true));
            table.put(new ParameterDouble("actual_threshold", 0.0));
            table.put(new ParameterInt("variance_estimator", MAD));
            table.put(new ParameterDouble("threshold_multiplier", 3.6));
            table.put(new ParameterInt("thresholding_type", HARD_THRESHOLDING));
            table.put(new ParameterBoolean("threshold_low_pass", false));
            table.put(new ParameterBoolean("do_Bayes", false));
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
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }


}
