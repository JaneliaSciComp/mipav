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
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. Algorithms are executed in their own thread.
 *
 * @see  MultiResolutionBilateralFilter
 */
public class JDialogMultiResolutionBilateralFilter extends JDialogScriptableBase
        implements AlgorithmInterface, ScriptableActionInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private MultiResolutionBilateralFilter mrbFilterAlgo;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;
    
    private JPanel paramPanel;
    
    private JRadioButton newImage;
    
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private ModelImage image; // source image
    
    private ModelImage resultImage = null;
    
    private ViewUserInterface userInterface;
    
    private String[] titles;
    
    private String nameString;
    
    private PyWavelets.WAVELET_NAME wavelet_name = PyWavelets.WAVELET_NAME.DB;
    
    private int wavelet_order = 8;
    
	// The number of wavelet decomposition levels to use.
	private int wavelet_levels = 4;
    
    private JLabel labelName;
    
    private JComboBox<String> comboBoxName;
    
    private JLabel labelOrder;
    
    private JComboBox<String> comboBoxOrder;
    
    private JLabel labelLevels;
    
    private JComboBox<String> comboBoxLevels;
    
    private JCheckBox estimateNoiseCheckBox;
    
    private JLabel labelNoiseStandardDeviation;
    
    private JTextField textNoiseStandardDeviation;
    
    private boolean estimateNoiseStandardDeviation = true;
	private double noiseStandardDeviation = 10.0;
	
	// PyWavelets.FILTER_SOFT or PyWavelets.FILTER_HARD
	// An optional argument to choose the type of denoising performed. It
    // noted that choosing soft thresholding given additive noise finds the
    // best approximation of the original image.
	private int filterType = PyWavelets.FILTER_SOFT;
	
	private ButtonGroup thresholdGroup;
	
	private JRadioButton hardButton;
	
	private JRadioButton softButton;
	
	private int kernelWidth;
	
	private JLabel labelKernel;
	
	private JTextField textKernel;
	
	private double sigmaColor = 0.5;
	private double sigmaSpace = 1.8;
	
	private JLabel labelColor;
	private JTextField textColor;
	
	private JLabel labelSpace;
	private JTextField textSpace;
	
	private String lastNameString = "Daubechies";
	
	private GridBagConstraints gbc2;
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMultiResolutionBilateralFilter() { }

    /**
     * Creates a new JDialogMultiResolutionBilateralFilter object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMultiResolutionBilateralFilter(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        setVisible(true);
    }
    
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (source == estimateNoiseCheckBox) {
        	estimateNoiseStandardDeviation = estimateNoiseCheckBox.isSelected();
        	labelNoiseStandardDeviation.setEnabled(!estimateNoiseStandardDeviation);
        	textNoiseStandardDeviation.setEnabled(!estimateNoiseStandardDeviation);
        } else if (source == comboBoxName) {
        	String nameString = (String) comboBoxName.getSelectedItem();
        	if (!nameString.equals(lastNameString)) {
        		lastNameString = nameString;
        		comboBoxOrder = buildWaveletOrderComboBox(nameString);
        		gbc2.gridx = 1;
        		gbc2.gridy = 1;
                paramPanel.add(comboBoxOrder, gbc2);
        	}
        } else { // else if (source == thresholdCheckbox)
            super.actionPerformed(event);
        }
    }

    
    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
    	int i;
        setForeground(Color.black);

        setTitle("Multiresolution Bilateral Filter");

        JPanel mainPanel;
        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        mainPanel.add(paramPanel, gbc);

        gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        
        labelName = new JLabel("Wavelet family name");
        labelName.setFont(serif12);
        labelName.setForeground(Color.black);
        paramPanel.add(labelName, gbc2);
        
        comboBoxName = buildWaveletNameComboBox();
        gbc2.gridx = 1;
        paramPanel.add(comboBoxName, gbc2);
        
        labelOrder = new JLabel("Wavelet order");
        labelOrder.setFont(serif12);
        labelOrder.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        paramPanel.add(labelOrder, gbc2);
     
        comboBoxOrder = buildWaveletOrderComboBox("Daubechies");
        gbc2.gridx = 1;
        paramPanel.add(comboBoxOrder, gbc2);
        
        labelLevels = new JLabel("Levels");
        labelLevels.setFont(serif12);
        labelLevels.setForeground(Color.black);
        labelLevels.setEnabled(false);
        gbc2.gridx = 0;
        gbc2.gridy++;
        paramPanel.add(labelLevels, gbc2);
        
        comboBoxLevels = new JComboBox<String>();
        comboBoxLevels.setFont(serif12);
        comboBoxLevels.setBackground(Color.white);
        for (i = 1; i <= 10; i++) {
            comboBoxLevels.addItem(String.valueOf(i));	 
        }
        comboBoxLevels.setSelectedIndex(3);
        comboBoxLevels.addActionListener(this);
        comboBoxLevels.setEnabled(false);
        gbc2.gridx = 1;
        paramPanel.add(comboBoxLevels, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy++;
        gbc2.gridwidth = 2;

        estimateNoiseCheckBox = new JCheckBox("Estimate Gaussian noise standard deviation");
        estimateNoiseCheckBox.setFont(serif12);
        estimateNoiseCheckBox.addActionListener(this);
        paramPanel.add(estimateNoiseCheckBox, gbc2);
        estimateNoiseCheckBox.setEnabled(true);
        estimateNoiseCheckBox.setSelected(true);

        gbc2.gridx = 0;
        gbc2.gridy++;
        gbc2.gridwidth = 1;
        labelNoiseStandardDeviation = createLabel("Noise standard deviation ");
        labelNoiseStandardDeviation.setEnabled(false);
        paramPanel.add(labelNoiseStandardDeviation, gbc2);

        gbc2.gridx = 1;
        textNoiseStandardDeviation = createTextField("10.0");
        textNoiseStandardDeviation.setEnabled(false);
        paramPanel.add(textNoiseStandardDeviation, gbc2);
        
        thresholdGroup = new ButtonGroup();
        gbc2.gridx = 0;
        gbc2.gridy++;
        gbc2.gridwidth = 2;
        hardButton = new JRadioButton("Hard threshold", false);
        hardButton.setFont(serif12);
        thresholdGroup.add(hardButton);
        paramPanel.add(hardButton, gbc2);
        
        gbc2.gridy++;
        softButton = new JRadioButton("Soft threshold", true);
        softButton.setFont(serif12);
        thresholdGroup.add(softButton);
        paramPanel.add(softButton, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy++;
        gbc2.gridwidth = 1;
        labelKernel = createLabel("Kernel width");
        paramPanel.add(labelKernel, gbc2);
        
        gbc2.gridx = 1;
        textKernel = createTextField("5");
        paramPanel.add(textKernel, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy++;
        labelColor = createLabel("Color standard deviation");
        paramPanel.add(labelColor, gbc2);
        
        gbc2.gridx = 1;
        textColor = createTextField("0.5");
        paramPanel.add(textColor, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy++;
        labelSpace = createLabel("Space standard deviation");
        paramPanel.add(labelSpace, gbc2);
        
        gbc2.gridx = 1;
        textSpace = createTextField("1.8");
        paramPanel.add(textSpace, gbc2);
        
        JPanel outputOptPanel = new JPanel(new GridLayout(1, 2));
        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        outputOptPanel.add(destinationPanel);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setBounds(10, 16, 120, 25);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage, BorderLayout.NORTH);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage, BorderLayout.CENTER);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(outputOptPanel, gbc);

        mainDialogPanel.add(mainPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buildButtons(), BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setResizable(true);
        // setVisible(true);

        System.gc();
    }
    
    private JComboBox<String> buildWaveletNameComboBox() {
    	final JComboBox<String> comboBox = new JComboBox<String>();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);
        comboBox.addItem("Biorthogonal");
        comboBox.addItem("Coiflets");
        comboBox.addItem("Daubechies");
        comboBox.addItem("Haar");
        comboBox.addItem("Reverse biorthogonal");
        comboBox.addItem("Symlets");
        comboBox.setSelectedIndex(2);
        comboBox.addActionListener(this);
        return comboBox;
    }
    
    private JComboBox<String> buildWaveletOrderComboBox(String family) {
    	int i;
    	final JComboBox<String> comboBox = new JComboBox<String>();
    	comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);
    	comboBox.removeAllItems();
    	if ((family.equals("Biorthogonal")) || (family.equals("Reverse biorthogonal"))) {
    		comboBox.addItem("11");
    		comboBox.addItem("13");
    		comboBox.addItem("15");
    		comboBox.addItem("22");
    		comboBox.addItem("24");
    		comboBox.addItem("26");
    		comboBox.addItem("28");
    		comboBox.addItem("31");
    		comboBox.addItem("33");
    		comboBox.addItem("35");
    		comboBox.addItem("37");
    		comboBox.addItem("39");
    		comboBox.addItem("44");
    		comboBox.addItem("55");
    		comboBox.addItem("68");
    	}
    	else if (family.equals("Coiflets")) {
    	    for (i = 0; i < 17; i++) {
    	    	comboBox.addItem(String.valueOf(i+1));
    	    }
    	}
    	else if (family.equals("Daubechies")) {
    		for (i = 0; i < 38; i++) {
    	    	comboBox.addItem(String.valueOf(i+1));
    	    }	
    	}
    	else if (family.equals("Haar")) {
    		comboBox.addItem("1");
    	}
    	else if (family.equals("Symlets")) {
    	    for (i = 0; i < 19; i++) {
    	    	comboBox.addItem(String.valueOf(i+2));	
    	    }
    	}
    	return comboBox;
    }
    
    /**
     * Once all the necessary variables are set, call the Nonlocal Means filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_multiResBilateral");
        int[] destExtents;

        destExtents = new int[2];
        destExtents[0] = image.getExtents()[0]; // X dim
        destExtents[1] = image.getExtents()[1]; // Y dim     

        if (displayLoc == NEW) {

            try {

                // Make result image of float type
                if (image.isColorImage()) {
                    resultImage = new ModelImage(image.getType(), destExtents, name);
                } else {
                    resultImage = new ModelImage(ModelImage.DOUBLE, destExtents, name);
                }

                // Make algorithm
                mrbFilterAlgo = new MultiResolutionBilateralFilter(resultImage, image, wavelet_name, wavelet_order,
                                         wavelet_levels, estimateNoiseStandardDeviation, noiseStandardDeviation,
                                         filterType, kernelWidth, sigmaColor, sigmaSpace);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                mrbFilterAlgo.addListener(this);
                createProgressBar(image.getImageName(), mrbFilterAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (mrbFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    mrbFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Multiresolution Bilateral Filter: unable to allocate enough memory");

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
            	// Make algorithm
                mrbFilterAlgo = new MultiResolutionBilateralFilter(null, image, wavelet_name, wavelet_order,
                                         wavelet_levels, estimateNoiseStandardDeviation, noiseStandardDeviation,
                                         filterType, kernelWidth, sigmaColor, sigmaSpace);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                mrbFilterAlgo.addListener(this);
                createProgressBar(image.getImageName(), mrbFilterAlgo);

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

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (mrbFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    mrbFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Multiresolution Bilateral Filter: unable to allocate enough memory");

                return;
            }
        }
    }

    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof MultiResolutionBilateralFilter) {
            image.clearMask();

            if ((mrbFilterAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
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
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        mrbFilterAlgo.finalize();
        mrbFilterAlgo = null;
        dispose();
    }
    
    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        
        nameString = (String) comboBoxName.getSelectedItem();
        if (nameString.equals("Biorthogonal")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.BIOR;
    	}
    	else if (nameString.equals("Coiflets")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.COIF;
    	}
    	else if (nameString.equals("Daubechies")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.DB;
    	}
    	else if (nameString.equals("Haar")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.HAAR;
    	}
    	else if (nameString.equals("Reverse biorthogonal")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.RBIO;
    	}
    	else if (nameString.equals("Symlets")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.SYM;
    	}
        
        tmpStr = (String) comboBoxOrder.getSelectedItem();
    	wavelet_order = Integer.valueOf(tmpStr).intValue();
    	
    	tmpStr = (String)comboBoxLevels.getSelectedItem();
    	wavelet_levels = Integer.parseInt(tmpStr);
    	
    	estimateNoiseStandardDeviation = estimateNoiseCheckBox.isSelected();

        if (!estimateNoiseStandardDeviation) {
	        tmpStr = textNoiseStandardDeviation.getText();
	
	        if (testParameter(tmpStr, 0.001, 1000.0)) {
	            noiseStandardDeviation = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            MipavUtil.displayError("Radius must be between 0.001 and 1000.0");
	            textNoiseStandardDeviation.requestFocus();
	            textNoiseStandardDeviation.selectAll();
	
	            return false;
	        }
        } // if (!estimateNoiseStandardDeviation)
        
        if (hardButton.isSelected()) {
        	filterType = PyWavelets.FILTER_HARD;
        }
        else {
        	filterType = PyWavelets.FILTER_SOFT;
        }
        
        tmpStr = textKernel.getText();

        if (testParameter(tmpStr, 3, 99)) {
            kernelWidth = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Kernel width must be between 3 and 99");
            textKernel.requestFocus();
            textKernel.selectAll();

            return false;
        }
        
        if ((kernelWidth % 2) == 0) {
            MipavUtil.displayError("Kernel width be an odd number");
            textKernel.requestFocus();
            textKernel.selectAll();
            return false;
        }
        
        tmpStr = textColor.getText();
    	
        if (testParameter(tmpStr, 0.001, 1000.0)) {
            sigmaColor = Double.valueOf(tmpStr).doubleValue();
        } else {
            MipavUtil.displayError("Color standard deviation be between 0.001 and 1000.0");
            textColor.requestFocus();
            textColor.selectAll();

            return false;
        }
        
        tmpStr = textSpace.getText();
    	
        if (testParameter(tmpStr, 0.001, 1000.0)) {
            sigmaSpace = Double.valueOf(tmpStr).doubleValue();
        } else {
            MipavUtil.displayError("Space standard deviation be between 0.001 and 1000.0");
            textSpace.requestFocus();
            textSpace.selectAll();

            return false;
        }
        
        return true;
    }
    
    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
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
            table.put(new ParameterString("name_string", "Daubechies"));
            table.put(new ParameterInt("w_order", 8));
            table.put(new ParameterInt("w_levels", 4));
            table.put(new ParameterBoolean("estimate_noise_std", true));
            table.put(new ParameterDouble("noise_standard_deviation",10.0));
            table.put(new ParameterInt("f_type", 1));
            table.put(new ParameterInt("k_width", 5));
            table.put(new ParameterDouble("sigma_color", 0.5));
            table.put(new ParameterDouble("sigma_space", 1.8));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }
        
        nameString = scriptParameters.getParams().getString("name_string");
        if (nameString.equals("Biorthogonal")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.BIOR;
    	}
    	else if (nameString.equals("Coiflets")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.COIF;
    	}
    	else if (nameString.equals("Daubechies")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.DB;
    	}
    	else if (nameString.equals("Haar")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.HAAR;
    	}
    	else if (nameString.equals("Reverse biorthogonal")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.RBIO;
    	}
    	else if (nameString.equals("Symlets")) {
    		wavelet_name = PyWavelets.WAVELET_NAME.SYM;
    	}
        wavelet_order = scriptParameters.getParams().getInt("w_order");
        wavelet_levels = scriptParameters.getParams().getInt("w_levels");
        estimateNoiseStandardDeviation = scriptParameters.getParams().getBoolean("estimate_noise_std");
        noiseStandardDeviation = scriptParameters.getParams().getDouble("noise_standard_deviation");
        filterType = scriptParameters.getParams().getInt("f_type");
        kernelWidth = scriptParameters.getParams().getInt("k_width");
        sigmaColor = scriptParameters.getParams().getDouble("sigma_color");
        sigmaSpace = scriptParameters.getParams().getDouble("sigma_space");
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("name_string", nameString));
        scriptParameters.getParams().put(ParameterFactory.newParameter("w_order", wavelet_order));
        scriptParameters.getParams().put(ParameterFactory.newParameter("w_levels", wavelet_levels));
        scriptParameters.getParams().put(ParameterFactory.newParameter("estimate_noise_std", estimateNoiseStandardDeviation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("noise_standard_deviation", noiseStandardDeviation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("f_type", filterType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("k_width", kernelWidth));
        scriptParameters.getParams().put(ParameterFactory.newParameter("sigma_color", sigmaColor));
        scriptParameters.getParams().put(ParameterFactory.newParameter("sigma_space", sigmaSpace));
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
                return image.getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
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
                return new String("Applies a Multiresolution Bilateral filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Multiresolution Bilateral filter.");
            }

            public String getShortLabel() {
                return new String("MultiresBilateral");
            }

            public String getLabel() {
                return new String("Multiresolution Bilateral");
            }

            public String getName() {
                return new String("Multiresolution Bilateral");
            }
        };
    }
}
