package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 *
 */
public class JDialogPyWavelets extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID =;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private PyWavelets waveletAlgo;

    /** Source image. */
    private ModelImage image;

    /** Result image. */
    private ModelImage resultImage = null;
    
    private int tType;
    
    private PyWavelets.WAVELET_NAME names[];
    
    private int orders[];
    
    private PyWavelets.MODE modes[];
    
    private int axes[];
    
    private int filterType[];
    
    private double filterVal1[];
    
    private double filterVal2[];
    
    private boolean showTransform;
    
    private boolean showFilteredTransform;
    
    private int levels;
    
    private int start_level;
    
    private ButtonGroup transformTypeGroup;
    
    private JRadioButton singleLevelDWTButton;
    
    private JRadioButton multiLevelDWTButton;
    
    private JRadioButton SWTButton;
    
    private int SINGLE_LEVEL_DWT = 1; // Any number of axes
    private int MULTILEVEL_DWT = 2; // 2 axes for wavedec2 and 3 axes for wavedec3
    private int SWT = 3;
    
    private JLabel labelLevels;
    private JTextField textLevels;
    
    private JLabel labelStartLevel;
    private JTextField textStartLevel;
    
    private JCheckBox xAxisCheckBox;
    private JCheckBox yAxisCheckBox;
    private JCheckBox zAxisCheckBox;
    private boolean doX = false;
    private boolean doY = false;
    private boolean doZ = false;
    
    private JCheckBox transformCheckBox;
    private JCheckBox filteredCheckBox;
    /** Tabbed pane */
    private JTabbedPane tabbedPane = null;
    
    private JLabel labelModeX;
    private JLabel labelModeY;
    private JLabel labelModeZ;
    private JComboBox comboBoxModeX;
    private JComboBox comboBoxModeY;
    private JComboBox comboBoxModeZ;
    
    /** DOCUMENT ME! */
    private JPanel paramsPanel;
    
    
    private JLabel mainLabel;
    
    private JLabel mainLabel2;
    
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogPyWavelets() { }

    /**
     * Construct the PyWavelets dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogPyWavelets(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
        //loadDefaults();
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
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("PyWavelets");
        } else if ((source == singleLevelDWTButton) || (source == multiLevelDWTButton) || (source == SWTButton)) {
        	if (singleLevelDWTButton.isSelected()) {
        	    labelLevels.setEnabled(false);
        	    textLevels.setEnabled(false);
        	    labelStartLevel.setEnabled(false);
        	    textStartLevel.setEnabled(false);
        	    if (xAxisCheckBox.isSelected()) {
        	    	labelModeX.setEnabled(true);
            		comboBoxModeX.setEnabled(true);	
        	    }
        	    if (yAxisCheckBox.isSelected()) {
        	    	labelModeY.setEnabled(true);
            		comboBoxModeY.setEnabled(true);	
        	    }
        	    if (image.getNDims() > 2) {
        	    	if (zAxisCheckBox.isSelected()) {
            	    	labelModeZ.setEnabled(true);
                		comboBoxModeZ.setEnabled(true);	
            	    }	
        	    }
        	}
        	else if (multiLevelDWTButton.isSelected()) {
        		labelLevels.setEnabled(true);
        	    textLevels.setEnabled(true);
        	    labelStartLevel.setEnabled(false);
        	    textStartLevel.setEnabled(false);
        	    if (xAxisCheckBox.isSelected()) {
        	        labelModeX.setEnabled(true);
        		    comboBoxModeX.setEnabled(true);
        	    }
        	    if (yAxisCheckBox.isSelected()) {
        	    	labelModeY.setEnabled(true);
            		comboBoxModeY.setEnabled(true);	
        	    }
        	    if (image.getNDims() > 2) {
        	    	if (zAxisCheckBox.isSelected()) {
            	    	labelModeZ.setEnabled(true);
                		comboBoxModeZ.setEnabled(true);	
            	    }	
        	    }
        	}
        	else { // SWTButton.isSelected()
        		labelLevels.setEnabled(true);
        	    textLevels.setEnabled(true);
        	    labelStartLevel.setEnabled(true);
        	    textStartLevel.setEnabled(true);
        	    labelModeX.setEnabled(false);
        	    comboBoxModeX.setEnabled(false);
        	    labelModeY.setEnabled(false);
        	    comboBoxModeY.setEnabled(false);
        	    if (image.getNDims() > 2) {
        	        labelModeZ.setEnabled(false);
        	        comboBoxModeZ.setEnabled(false);
        	    }
        	}
        } else if ((source == xAxisCheckBox) || (source == yAxisCheckBox) || ((image.getNDims() > 2) && (source == zAxisCheckBox))) {
        	if (xAxisCheckBox.isSelected() && (singleLevelDWTButton.isSelected() || multiLevelDWTButton.isSelected())) {
        		labelModeX.setEnabled(true);
        		comboBoxModeX.setEnabled(true);
        	}
        	else {
        		labelModeX.setEnabled(false);
        		comboBoxModeX.setEnabled(false);	
        	}
           	if (yAxisCheckBox.isSelected() && (singleLevelDWTButton.isSelected() || multiLevelDWTButton.isSelected())) {
        		labelModeY.setEnabled(true);
        		comboBoxModeY.setEnabled(true);
        	}
        	else {
        		labelModeY.setEnabled(false);
        		comboBoxModeY.setEnabled(false);	
        	}
        	if (image.getNDims() > 2) {
        		if (zAxisCheckBox.isSelected() && (singleLevelDWTButton.isSelected() || multiLevelDWTButton.isSelected())) {
            		labelModeZ.setEnabled(true);
            		comboBoxModeZ.setEnabled(true);
            	}
            	else {
            		labelModeZ.setEnabled(false);
            		comboBoxModeZ.setEnabled(false);	
            	}
        	}
        } else {
            super.actionPerformed(event);
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

        if (algorithm instanceof PyWavelets) {
            Preferences.debug("PyWavelets: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((waveletAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                if (resultImage.isColorImage()) {
                    updateFileInfo(image, resultImage);
                }

                resultImage.clearMask();

                try {
                    openNewFrame(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        } // if (algorithm instanceof AlgorithmBarrelDistortion)

        

        if (waveletAlgo != null) {
            waveletAlgo.finalize();
            waveletAlgo = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    
    /**
     * Once all the necessary variables are set, call the Barrel/Pincushion Distortion
     * Correction algorithm based on what type of image this is
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_filter");
        
        try {

            // Make result image
        	resultImage = new ModelImage(ModelImage.DOUBLE, image.getExtents(), name);

            // Make algorithm
            waveletAlgo = new PyWavelets(resultImage, image, tType, names, orders, modes, axes, filterType, filterVal1, filterVal2,
            		showTransform, showFilteredTransform, levels, start_level);

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

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("PyWavelets: unable to allocate enough memory");

            return;
        }
            
    }

    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {

        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        
        
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);

        
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("PyWavelets");
        final JPanel transformTypePanel = buildTransformTypePanel();
        final JPanel waveletPanel = buildWaveletPanel();
        final JPanel filterPanel = buildFilterPanel();
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        tabbedPane.addTab("Transform type", transformTypePanel);
        tabbedPane.addTab("Wavelets", waveletPanel);
        tabbedPane.addTab("Filters", filterPanel);
        //tabbedPane.addChangeListener(this);
        
        final JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildButtons());

        getContentPane().add(tabbedPane);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        
        pack();
        setVisible(true);
        setResizable(false);

        System.gc();
    }
    
    private JPanel buildTransformTypePanel() {
    	 final JPanel transformTypePanel = new JPanel(new GridBagLayout());
    	 GridBagConstraints gbc = new GridBagConstraints();
         gbc.gridwidth = 3;
         gbc.anchor = GridBagConstraints.WEST;
         gbc.weightx = 1;

    	 transformTypePanel.setBorder(buildTitledBorder("Transform"));
    	 transformTypeGroup = new ButtonGroup();
         singleLevelDWTButton = new JRadioButton("Single level discrete wavelet transform", true);
         singleLevelDWTButton.setFont(serif12);
         singleLevelDWTButton.setForeground(Color.black);
         singleLevelDWTButton.addActionListener(this);
         transformTypeGroup.add(singleLevelDWTButton);
         gbc.gridx = 0;
         gbc.gridy = 0;
         transformTypePanel.add(singleLevelDWTButton,gbc);
         
         multiLevelDWTButton = new JRadioButton("Multilevel discrete wavelet transform", false);
         multiLevelDWTButton.setFont(serif12);
         multiLevelDWTButton.setForeground(Color.black);
         multiLevelDWTButton.addActionListener(this);
         transformTypeGroup.add(multiLevelDWTButton);
         gbc.gridy = 1;
         transformTypePanel.add(multiLevelDWTButton, gbc);
         
         SWTButton = new JRadioButton("Stationary wavelet transform", false);
         SWTButton.setFont(serif12);
         SWTButton.setForeground(Color.black);
         SWTButton.addActionListener(this);
         transformTypeGroup.add(SWTButton);
         gbc.gridy = 2;
         transformTypePanel.add(SWTButton, gbc);
         
         labelLevels = new JLabel("Levels");
         labelLevels.setFont(serif12);
         labelLevels.setForeground(Color.black);
         labelLevels.setEnabled(false);
         gbc.gridy = 3;
         transformTypePanel.add(labelLevels, gbc);
         
         textLevels = new JTextField(10);
         textLevels.setText("2");
         textLevels.setFont(serif12);
         textLevels.setForeground(Color.black);
         textLevels.setEnabled(false);
         gbc.gridx = 1;
         transformTypePanel.add(textLevels, gbc);
         
         labelStartLevel = new JLabel("Start level");
         labelStartLevel.setFont(serif12);
         labelStartLevel.setForeground(Color.black);
         labelStartLevel.setEnabled(false);
         gbc.gridx = 0;
         gbc.gridy = 4;
         transformTypePanel.add(labelStartLevel, gbc);
         
         textStartLevel = new JTextField(10);
         textStartLevel.setText("0");
         textStartLevel.setFont(serif12);
         textStartLevel.setForeground(Color.black);
         textStartLevel.setEnabled(false);
         gbc.gridx = 1;
         transformTypePanel.add(textStartLevel, gbc);
         
         xAxisCheckBox = new JCheckBox("Transform along x axis");
         xAxisCheckBox.setFont(serif12);
         xAxisCheckBox.setForeground(Color.black);
         xAxisCheckBox.addActionListener(this);
         xAxisCheckBox.setSelected(true);
         gbc.gridx = 0;
         gbc.gridy = 5;
         transformTypePanel.add(xAxisCheckBox, gbc);
         
         yAxisCheckBox = new JCheckBox("Transform along y axis");
         yAxisCheckBox.setFont(serif12);
         yAxisCheckBox.setForeground(Color.black);
         yAxisCheckBox.addActionListener(this);
         yAxisCheckBox.setSelected(true);
         gbc.gridx = 0;
         gbc.gridy = 6;
         transformTypePanel.add(yAxisCheckBox, gbc);
         
         if (image.getNDims() > 2) {
        	 zAxisCheckBox = new JCheckBox("Transform along z axis");
             zAxisCheckBox.setFont(serif12);
             zAxisCheckBox.setForeground(Color.black);
             zAxisCheckBox.addActionListener(this);
             zAxisCheckBox.setSelected(true);
             gbc.gridx = 0;
             gbc.gridy = 7;
             transformTypePanel.add(yAxisCheckBox, gbc);	 
         }
         
         transformCheckBox = new JCheckBox("Show transform images");
         transformCheckBox.setFont(serif12);
         transformCheckBox.setForeground(Color.black);
         transformCheckBox.setSelected(false);
         gbc.gridx = 0;
         gbc.gridy++;
         transformTypePanel.add(transformCheckBox, gbc);
         
         filteredCheckBox = new JCheckBox("Show filtered transform images");
         filteredCheckBox.setFont(serif12);
         filteredCheckBox.setForeground(Color.black);
         filteredCheckBox.setSelected(false);
         gbc.gridx = 0;
         gbc.gridy++;
         transformTypePanel.add(filteredCheckBox, gbc);
         
         labelModeX = new JLabel("X axis mode");
         labelModeX.setFont(serif12);
         labelModeX.setForeground(Color.black);
         labelModeX.setEnabled(true);
         gbc.gridx = 0;
         gbc.gridy++;
         transformTypePanel.add(labelModeX, gbc);
         
         comboBoxModeX = buildModeComboBox();
         gbc.gridx = 1;
         transformTypePanel.add(comboBoxModeX, gbc);
         
         labelModeY = new JLabel("Y axis mode");
         labelModeY.setFont(serif12);
         labelModeY.setForeground(Color.black);
         labelModeY.setEnabled(true);
         gbc.gridx = 0;
         gbc.gridy++;
         transformTypePanel.add(labelModeY, gbc);
         
         comboBoxModeY = buildModeComboBox();
         gbc.gridx = 1;
         transformTypePanel.add(comboBoxModeY, gbc);
         
         if (image.getNDims() > 2) {
        	 labelModeZ = new JLabel("Z axis mode");
             labelModeZ.setFont(serif12);
             labelModeZ.setForeground(Color.black);
             labelModeZ.setEnabled(true);
             gbc.gridx = 0;
             gbc.gridy++;
             transformTypePanel.add(labelModeZ, gbc);
             
             comboBoxModeZ = buildModeComboBox();
             gbc.gridx = 1;
             transformTypePanel.add(comboBoxModeZ, gbc);	 
         }
    	 return transformTypePanel;
    }
    
    private JPanel buildWaveletPanel() {
   	    final JPanel waveletPanel = new JPanel(new GridBagLayout());
   	    GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 3;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

   	    waveletPanel.setBorder(buildTitledBorder("Wavelets"));
   	    return waveletPanel;
    }
    
    private JPanel buildFilterPanel() {
   	    final JPanel filterPanel = new JPanel(new GridBagLayout());
   	    GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 3;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

   	    filterPanel.setBorder(buildTitledBorder("Filters"));
   	    return filterPanel;
    }
    
    private JComboBox<String> buildModeComboBox() {
    	final JComboBox<String> comboBox = new JComboBox<String>();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);
        comboBox.addItem("Zeropad");
        comboBox.addItem("Symmetric");
        comboBox.addItem("Constant edge");
        comboBox.addItem("Smooth");
        comboBox.addItem("Periodic");
        comboBox.addItem("Periodization");
        comboBox.addItem("Reflect");
        comboBox.addItem("Antisymmetric");
        comboBox.addItem("Antireflect");
        comboBox.addItem("Max");
        comboBox.setSelectedIndex(1);
        return comboBox;
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        int i;
        if (singleLevelDWTButton.isSelected()) {
        	tType = SINGLE_LEVEL_DWT;
        }
        else if (multiLevelDWTButton.isSelected()) {
        	tType = MULTILEVEL_DWT;
        }
        else {
        	tType = SWT;
        }
        
        if ((tType == MULTILEVEL_DWT) || (tType == SWT)) {
        	tmpStr = textLevels.getText();
        	try {
        		levels = Integer.parseInt(tmpStr);
        	}
        	catch(NumberFormatException e) {
        		MipavUtil.displayError("Levels must be an integer");
        		textLevels.requestFocus();
        		textLevels.selectAll();
        		return false;
        	}
        	if (levels < 1) {
        		MipavUtil.displayError("Number of levels must be at least 1");
        		textLevels.requestFocus();
        		textLevels.selectAll();
        		return false;
        	}
        	else if (levels > 10) {
        		MipavUtil.displayError("Number of levels cannot exceed 10");
        		textLevels.requestFocus();
        		textLevels.selectAll();
        		return false;
        	}
        }
        
        if (tType == SWT) {
        	tmpStr = textStartLevel.getText();
        	try {
        		start_level = Integer.parseInt(tmpStr);
        	}
        	catch(NumberFormatException e) {
        		MipavUtil.displayError("Start level must be an integer");
        		textStartLevel.requestFocus();
        		textStartLevel.selectAll();
        		return false;
        	}
        	if (start_level < 0) {
        		MipavUtil.displayError("Start level must be at least 0");
        		textStartLevel.requestFocus();
        		textStartLevel.selectAll();
        		return false;
        	}
        	else if (start_level > 9) {
        		MipavUtil.displayError("Start level cannot exceed 0");
        		textStartLevel.requestFocus();
        		textStartLevel.selectAll();
        		return false;
        	}
        }
        
        doX = xAxisCheckBox.isSelected();
        doY = yAxisCheckBox.isSelected();
        if (image.getNDims() > 2) {
        	doZ = zAxisCheckBox.isSelected();
        }
        
        if (doX && doY && doZ) {
        	axes = new int[]{0,1,2};
        }
        else if (doX && doY) {
        	axes = new int[]{0,1};
        }
        else if (doX && doZ) {
        	axes = new int[]{0,2};
        }
        else if (doY && doZ) {
        	axes = new int[]{1,2};
        }
        else if (doX) {
        	axes = new int[]{0};
        }
        else if (doY) {
        	axes = new int[]{1};
        }
        else if (doZ) {
        	axes = new int[]{2};
        }
        else {
        	MipavUtil.displayError("No axis has been selected");
        	return false;
        }
        
        showTransform = transformCheckBox.isSelected();
        
        showFilteredTransform = filteredCheckBox.isSelected();
        
        if ((tType == SINGLE_LEVEL_DWT) || (tType == MULTILEVEL_DWT)) {
	        modes = new PyWavelets.MODE[axes.length];
	        String modeString[] = new String[axes.length];
	        int index = 0;
	        if (doX) {
	        	modeString[index++] = (String) comboBoxModeX.getSelectedItem();
	        }
	        if (doY) {
	        	modeString[index++] = (String) comboBoxModeY.getSelectedItem();
	        }
	        if (doZ) {
	        	modeString[index] = (String) comboBoxModeZ.getSelectedItem();
	        }
	        for (i = 0; i < axes.length; i++) {
	        	if (modeString[i].equals("Zeropad")) {
	        		modes[i] = PyWavelets.MODE.MODE_ZEROPAD;
	        	}
	        	else if (modeString[i].equals("Symmetric")) {
	        		modes[i] = PyWavelets.MODE.MODE_SYMMETRIC;
	        	}
	        	else if (modeString[i].equals("Constant edge")) {
	        		modes[i] = PyWavelets.MODE.MODE_CONSTANT_EDGE;
	        	}
	        	else if (modeString[i].equals("Smooth")) {
	        		modes[i] = PyWavelets.MODE.MODE_SMOOTH;
	        	}
	        	else if (modeString[i].equals("Periodic")) {
	        		modes[i] = PyWavelets.MODE.MODE_PERIODIC;
	        	}
	        	else if (modeString[i].equals("Periodization")) {
	        		modes[i] = PyWavelets.MODE.MODE_PERIODIZATION;
	        	}
	        	else if (modeString[i].equals("Reflect")) {
	        		modes[i] = PyWavelets.MODE.MODE_REFLECT;
	        	}
	        	else if (modeString[i].equals("Antireflect")) {
	        		modes[i] = PyWavelets.MODE.MODE_ANTIREFLECT;
	        	}
	        	else if (modeString[i].equals("Antisymmetric")) {
	        		modes[i] = PyWavelets.MODE.MODE_ANTISYMMETRIC;
	        	}
	        	else if (modeString[i].equals("Max")) {
	        		modes[i] = PyWavelets.MODE.MODE_MAX;
	        	}
	        }
        } // if ((tType == SINGLE_LEVEL_DWT) || (tType == MULTILEVEL_DWT))
        return true;
    }
}
