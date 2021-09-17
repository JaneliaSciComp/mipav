package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
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
    private JComboBox<String> comboBoxLevels;
    
    private JLabel labelStartLevel;
    private JComboBox<String> comboBoxStartLevel;
    
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
    private JComboBox<String> comboBoxModeX;
    private JComboBox<String> comboBoxModeY;
    private JComboBox<String> comboBoxModeZ;
    
    private JLabel labelNameX;
    private JLabel labelNameY;
    private JLabel labelNameZ;
    private JComboBox<String> comboBoxNameX;
    private JComboBox<String> comboBoxNameY;
    private JComboBox<String> comboBoxNameZ;
    private JLabel labelOrderX;
    private JLabel labelOrderY;
    private JLabel labelOrderZ;
    private JComboBox<String> comboBoxOrderX;
    private JComboBox<String> comboBoxOrderY;
    private JComboBox<String> comboBoxOrderZ;
    private String lastNameStringX = "Daubechies";
    private String lastNameStringY = "Daubechies";
    private String lastNameStringZ = "Daubechies";
    
    GridBagConstraints gbc = new GridBagConstraints();
    JPanel waveletPanel;
    
    private int numComponents = 4;
    private JLabel labelComponents[];
    private JComboBox<String> comboBoxFilterType[];
    private JLabel labelVal1[];
    private JTextField textVal1[];
    private JLabel labelVal2[];
    private JTextField textVal2[];
    
    private final int FILTER_NONE = 0;
	private final int FILTER_SOFT = 1;
	private final int FILTER_NN_GARROTE = 2;
	private final int FILTER_HARD = 3;
	private final int FILTER_GREATER = 4;
	private final int FILTER_LESS = 5;
	private final int FILTER_THRESHOLD_FIRM = 6;
	
	private JCheckBox BayesCheckBox;
	private boolean doBayesShrinkThresholdComputation = false;
    

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
    	int i;
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
        	    comboBoxLevels.setEnabled(false);
        	    labelStartLevel.setEnabled(false);
        	    comboBoxStartLevel.setEnabled(false);
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
        	    comboBoxLevels.setEnabled(true);
        	    labelStartLevel.setEnabled(false);
        	    comboBoxStartLevel.setEnabled(false);
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
        	    comboBoxLevels.setEnabled(true);
        	    labelStartLevel.setEnabled(true);
        	    comboBoxStartLevel.setEnabled(true);
        	    labelModeX.setEnabled(false);
        	    comboBoxModeX.setEnabled(false);
        	    labelModeY.setEnabled(false);
        	    comboBoxModeY.setEnabled(false);
        	    if (image.getNDims() > 2) {
        	        labelModeZ.setEnabled(false);
        	        comboBoxModeZ.setEnabled(false);
        	    }
        	}
        	tabbedPane.removeTabAt(2);
        	final JPanel filterPanel = buildFilterPanel();
            JScrollPane filterScroll = new JScrollPane(filterPanel);
            filterScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
            filterScroll.setBorder(buildTitledBorder("Filters")); 
            tabbedPane.addTab("Filters", filterScroll);
        } else if ((source == xAxisCheckBox) || (source == yAxisCheckBox) || ((image.getNDims() > 2) && (source == zAxisCheckBox))) {
        	if (xAxisCheckBox.isSelected()) {
        		if (singleLevelDWTButton.isSelected() || multiLevelDWTButton.isSelected()) {
        		    labelModeX.setEnabled(true);
        		    comboBoxModeX.setEnabled(true);
        		}
        		labelNameX.setEnabled(true);
        		comboBoxNameX.setEnabled(true);
        		labelOrderX.setEnabled(true);
        		comboBoxOrderX.setEnabled(true);
        	}
        	else {
        		labelModeX.setEnabled(false);
        		comboBoxModeX.setEnabled(false);
        		labelNameX.setEnabled(false);
        		comboBoxNameX.setEnabled(false);
        		labelOrderX.setEnabled(false);
        		comboBoxOrderX.setEnabled(false);
        	}
           	if (yAxisCheckBox.isSelected()) {
           		if (singleLevelDWTButton.isSelected() || multiLevelDWTButton.isSelected()) {
        		    labelModeY.setEnabled(true);
        		    comboBoxModeY.setEnabled(true);
           		}
           		labelNameY.setEnabled(true);
        		comboBoxNameY.setEnabled(true);
        		labelOrderY.setEnabled(true);
        		comboBoxOrderY.setEnabled(true);
        	}
        	else {
        		labelModeY.setEnabled(false);
        		comboBoxModeY.setEnabled(false);
        		labelNameY.setEnabled(false);
        		comboBoxNameY.setEnabled(false);
        		labelOrderY.setEnabled(false);
        		comboBoxOrderY.setEnabled(false);
        	}
        	if (image.getNDims() > 2) {
        		if (zAxisCheckBox.isSelected()) {
        			if (singleLevelDWTButton.isSelected() || multiLevelDWTButton.isSelected()) {
            		    labelModeZ.setEnabled(true);
            		    comboBoxModeZ.setEnabled(true);
        			}
        			labelNameZ.setEnabled(true);
            		comboBoxNameZ.setEnabled(true);
            		labelOrderZ.setEnabled(true);
            		comboBoxOrderZ.setEnabled(true);
            	}
            	else {
            		labelModeZ.setEnabled(false);
            		comboBoxModeZ.setEnabled(false);
            		labelNameZ.setEnabled(false);
            		comboBoxNameZ.setEnabled(false);
            		labelOrderZ.setEnabled(false);
            		comboBoxOrderZ.setEnabled(false);
            	}
        	}
        	tabbedPane.removeTabAt(2);
        	final JPanel filterPanel = buildFilterPanel();
            JScrollPane filterScroll = new JScrollPane(filterPanel);
            filterScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
            filterScroll.setBorder(buildTitledBorder("Filters")); 
            tabbedPane.addTab("Filters", filterScroll);
        } else if (source == comboBoxNameX) {
        	String nameStringX = (String) comboBoxNameX.getSelectedItem();
        	if (!nameStringX.equals(lastNameStringX)) {
        		lastNameStringX = nameStringX;
        		comboBoxOrderX = buildWaveletOrderComboBox(nameStringX);
        		gbc.gridx = 1;
        		gbc.gridy = 1;
                waveletPanel.add(comboBoxOrderX, gbc);
        	}
        } else if (source == comboBoxNameY) {
        	String nameStringY = (String) comboBoxNameY.getSelectedItem();
        	if (!nameStringY.equals(lastNameStringY)) {
        		lastNameStringY = nameStringY;
        		comboBoxOrderY = buildWaveletOrderComboBox(nameStringY);
        		gbc.gridx = 1;
        		gbc.gridy = 3;
                waveletPanel.add(comboBoxOrderY, gbc);
        	}	
        } else if ((image.getNDims() > 2) && (source == comboBoxNameZ)) {
        	String nameStringZ = (String) comboBoxNameZ.getSelectedItem();
        	if (!nameStringZ.equals(lastNameStringZ)) {
        		lastNameStringZ = nameStringZ;
        		comboBoxOrderZ = buildWaveletOrderComboBox(nameStringZ);
        		gbc.gridx = 1;
        		gbc.gridy = 5;
                waveletPanel.add(comboBoxOrderZ, gbc);
        	}
        } else if ((source == comboBoxLevels) || (source == comboBoxStartLevel)){
        	tabbedPane.removeTabAt(2);
        	final JPanel filterPanel = buildFilterPanel();
            JScrollPane filterScroll = new JScrollPane(filterPanel);
            filterScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
            filterScroll.setBorder(buildTitledBorder("Filters")); 
            tabbedPane.addTab("Filters", filterScroll);
        } else {
        	for (i = 0; i < numComponents; i++) {
        		if (source == comboBoxFilterType[i]) {
        			String selection = (String)comboBoxFilterType[i].getSelectedItem();
        			if (selection.equals("THRESHOLD_FIRM")) {
        				labelVal1[i].setText("Low value");
        				labelVal2[i].setText("High value");
        			}
        			else {
        				labelVal1[i].setText("Value");
        				labelVal2[i].setText("Substitute");
        			}
        			if (selection.equals("NONE")) {
        				labelVal1[i].setEnabled(false);
        				labelVal2[i].setEnabled(false);
        				textVal1[i].setEnabled(false);
        				textVal2[i].setEnabled(false);
        			}
        			else {
        				labelVal1[i].setEnabled(true);
        				labelVal2[i].setEnabled(true);
        				textVal1[i].setEnabled(true);
        				textVal2[i].setEnabled(true);	
        			}
        		}
        	}
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
            		showTransform, showFilteredTransform, levels, start_level, doBayesShrinkThresholdComputation);

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
        waveletPanel = buildWaveletPanel();
        final JPanel filterPanel = buildFilterPanel();
        JScrollPane filterScroll = new JScrollPane(filterPanel);
        filterScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        filterScroll.setBorder(buildTitledBorder("Filters"));   
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        tabbedPane.addTab("Transform type", transformTypePanel);
        tabbedPane.addTab("Wavelets", waveletPanel);
        tabbedPane.addTab("Filters", filterScroll);
        //tabbedPane.addChangeListener(this);
        
        final JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildButtons());

        getContentPane().add(tabbedPane);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        
        pack();
        setVisible(true);
        setResizable(true);

        System.gc();
    }
    
    private JPanel buildTransformTypePanel() {
    	 int i;
    	 final JPanel transformTypePanel = new JPanel(new GridBagLayout());
    	 GridBagConstraints gbc = new GridBagConstraints();
         gbc.gridwidth = 1;
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
         
         comboBoxLevels = new JComboBox<String>();
         comboBoxLevels.setFont(serif12);
         comboBoxLevels.setBackground(Color.white);
         for (i = 1; i <= 10; i++) {
             comboBoxLevels.addItem(String.valueOf(i));	 
         }
         comboBoxLevels.setSelectedIndex(1);
         comboBoxLevels.addActionListener(this);
         comboBoxLevels.setEnabled(false);
         gbc.gridx = 1;
         transformTypePanel.add(comboBoxLevels, gbc);
         
         labelStartLevel = new JLabel("Start level");
         labelStartLevel.setFont(serif12);
         labelStartLevel.setForeground(Color.black);
         labelStartLevel.setEnabled(false);
         gbc.gridx = 0;
         gbc.gridy = 4;
         transformTypePanel.add(labelStartLevel, gbc);
         
         comboBoxStartLevel = new JComboBox<String>();
         comboBoxStartLevel.setFont(serif12);
         comboBoxStartLevel.setBackground(Color.white);
         for (i = 0; i <= 9; i++) {
             comboBoxStartLevel.addItem(String.valueOf(i));	 
         }
         comboBoxStartLevel.setSelectedIndex(0);
         comboBoxStartLevel.addActionListener(this);
         comboBoxStartLevel.setEnabled(false);
         gbc.gridx = 1;
         transformTypePanel.add(comboBoxStartLevel, gbc);
         
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
             transformTypePanel.add(zAxisCheckBox, gbc);	 
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
         
         BayesCheckBox = new JCheckBox("Bayes shrink threshold computation");
         BayesCheckBox.setFont(serif12);
         BayesCheckBox.setForeground(Color.black);
         BayesCheckBox.setSelected(false);
         gbc.gridx = 0;
         gbc.gridy++;
         transformTypePanel.add(BayesCheckBox, gbc);
    	 return transformTypePanel;
    }
    
    private JPanel buildWaveletPanel() {
    	final JPanel waveletPanel = new JPanel(new GridBagLayout());
    	gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

   	    waveletPanel.setBorder(buildTitledBorder("Wavelets"));
   	    labelNameX = new JLabel("X axis wavelet family name");
        labelNameX.setFont(serif12);
        labelNameX.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 0;
        waveletPanel.add(labelNameX, gbc);
        
        comboBoxNameX = buildWaveletNameComboBox();
        gbc.gridx = 1;
        waveletPanel.add(comboBoxNameX, gbc);
   	    
   	    labelOrderX = new JLabel("X axis wavelet order");
        labelOrderX.setFont(serif12);
        labelOrderX.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 1;
        waveletPanel.add(labelOrderX, gbc);
     
        comboBoxOrderX = buildWaveletOrderComboBox("Daubechies");
        gbc.gridx = 1;
        waveletPanel.add(comboBoxOrderX, gbc);
        
        labelNameY = new JLabel("Y axis wavelet family name");
        labelNameY.setFont(serif12);
        labelNameY.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 2;
        waveletPanel.add(labelNameY, gbc);
        
        comboBoxNameY = buildWaveletNameComboBox();
        gbc.gridx = 1;
        waveletPanel.add(comboBoxNameY, gbc);
   	    
   	    labelOrderY = new JLabel("Y axis wavelet order");
        labelOrderY.setFont(serif12);
        labelOrderY.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 3;
        waveletPanel.add(labelOrderY, gbc);
     
        comboBoxOrderY = buildWaveletOrderComboBox("Daubechies");
        gbc.gridx = 1;
        waveletPanel.add(comboBoxOrderY, gbc);
        
        if (image.getNDims() > 2) {
        	labelNameZ = new JLabel("Z axis wavelet family name");
            labelNameZ.setFont(serif12);
            labelNameZ.setForeground(Color.black);
            gbc.gridx = 0;
            gbc.gridy = 4;
            waveletPanel.add(labelNameZ, gbc);
            
            comboBoxNameZ = buildWaveletNameComboBox();
            gbc.gridx = 1;
            waveletPanel.add(comboBoxNameZ, gbc);
       	    
       	    labelOrderZ = new JLabel("Z axis wavelet order");
            labelOrderZ.setFont(serif12);
            labelOrderZ.setForeground(Color.black);
            gbc.gridx = 0;
            gbc.gridy = 5;
            waveletPanel.add(labelOrderZ, gbc);
         
            comboBoxOrderZ = buildWaveletOrderComboBox("Daubechies");
            gbc.gridx = 1;
            waveletPanel.add(comboBoxOrderZ, gbc);	
        } // if (image.getNDims() > 2)
	    return waveletPanel;
    }
    
    private JPanel buildFilterPanel() {
    	int i;
    	String tmpStr;
   	    final JPanel filterPanel = new JPanel(new GridBagLayout());
   	    GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        filterPanel.setBorder(buildTitledBorder("Filters"));
        int axesTransformed = 0;
        if (xAxisCheckBox.isSelected()) {
        	axesTransformed++;
        }
        if (yAxisCheckBox.isSelected()) {
        	axesTransformed++;
        }
        if ((image.getNDims() > 2) && zAxisCheckBox.isSelected()) {
        	axesTransformed++;
        }
        
        if (multiLevelDWTButton.isSelected() || SWTButton.isSelected()) {
        	tmpStr = (String)comboBoxLevels.getSelectedItem();
        	levels = Integer.parseInt(tmpStr);
        }
        
        if (SWTButton.isSelected()) {
        	tmpStr = (String)comboBoxStartLevel.getSelectedItem();
        	start_level = Integer.parseInt(tmpStr);
        }

        if (singleLevelDWTButton.isSelected()) {
        	if ((image.getNDims() == 2) || ((image.getNDims() > 2) && (!zAxisCheckBox.isSelected()))) {
        	    if (axesTransformed == 2) {
        	         numComponents = 4; 
        	         labelComponents = new JLabel[numComponents];
        	         labelComponents[0] = new JLabel("A");
        	         labelComponents[1] = new JLabel("H");
        	         labelComponents[2] = new JLabel("V");
        	         labelComponents[3] = new JLabel("D");
        	    } // if (axesTransformed == 2)
        	    else {
        	    	numComponents = 2;
        	    	labelComponents = new JLabel[numComponents];
        	    	labelComponents[0] = new JLabel("A");
        	    	labelComponents[1] = new JLabel("D");
        	    }
        	} // if ((image.getNDims() == 2) || ((image.getNDims() > 2) && (!zAxisCheckBox.isSelected())))
        	else if (axesTransformed == 3) {
        		numComponents = 8;
        		labelComponents = new JLabel[numComponents];
        		labelComponents[0] = new JLabel("LLL");
        		labelComponents[1] = new JLabel("HLL");
        		labelComponents[2] = new JLabel("LHL");
        		labelComponents[3] = new JLabel("HLL");
        		labelComponents[4] = new JLabel("LLH");
        		labelComponents[5] = new JLabel("HLH");
        		labelComponents[6] = new JLabel("LHH");
        		labelComponents[7] = new JLabel("HHH");
        	}
        	else if (axesTransformed == 2) {
        		numComponents = 4;
        		labelComponents = new JLabel[numComponents];
        		labelComponents[0] = new JLabel("LL");
        		labelComponents[1] = new JLabel("HL");
        		labelComponents[2] = new JLabel("LH");
        		labelComponents[3] = new JLabel("HL");
        	}
        	else {
        		numComponents = 2;
        		labelComponents = new JLabel[numComponents];
        		labelComponents[0] = new JLabel("L");
        		labelComponents[1] = new JLabel("H");
        	}
        } // if (singleLevelDWTButton.isSelected())
        else if (multiLevelDWTButton.isSelected()) {
        	if ((image.getNDims() == 2) || ((image.getNDims() > 2) && (!zAxisCheckBox.isSelected()))) {
        		if (axesTransformed == 2) {
        		    numComponents = 3*levels + 1;
        		    labelComponents = new JLabel[numComponents];
        		    labelComponents[0] = new JLabel("A"+String.valueOf(levels));
        		    for (i = levels; i >= 1; i--) {
        		    	labelComponents[3*(levels-i)+1] = new JLabel("H" + String.valueOf(i));
        		    	labelComponents[3*(levels-i)+2] = new JLabel("V" + String.valueOf(i));
        		    	labelComponents[3*(levels-i)+3] = new JLabel("D" + String.valueOf(i));
        		    }
        		} // if (axesTransformed == 2)
        		else {
        			numComponents = levels + 1;
        		    labelComponents = new JLabel[numComponents];
        		    labelComponents[0] = new JLabel("A"+String.valueOf(levels));
        		    for (i = levels; i >= 1; i--) {
        		    	labelComponents[(levels-i)+1] = new JLabel("D" + String.valueOf(i));
        		    }	
        		}
        	} // if ((image.getNDims() == 2) || ((image.getNDims() > 2) && (!zAxisCheckBox.isSelected())))
        	else if (axesTransformed == 3) {
        		numComponents = 7*levels + 1;
        		labelComponents = new JLabel[numComponents];
        		labelComponents[0] = new JLabel("LLL" + String.valueOf(levels));
        		for (i = levels; i >= 1; i--) {
        			labelComponents[7*(levels-i)+1] = new JLabel("HLL" + String.valueOf(i));
        			labelComponents[7*(levels-i)+2] = new JLabel("LHL" + String.valueOf(i));
        			labelComponents[7*(levels-i)+3] = new JLabel("HHL" + String.valueOf(i));
        			labelComponents[7*(levels-i)+4] = new JLabel("LLH" + String.valueOf(i));
        			labelComponents[7*(levels-i)+5] = new JLabel("HLH" + String.valueOf(i));
        			labelComponents[7*(levels-i)+6] = new JLabel("LHH" + String.valueOf(i));
        			labelComponents[7*(levels-i)+7] = new JLabel("HHH" + String.valueOf(i));
        		}
        	} // else if (axesTransformed == 3)
        	else if (axesTransformed == 2) {
        		numComponents = 3*levels + 1;
        		labelComponents = new JLabel[numComponents];
        		labelComponents[0] = new JLabel("LL" + String.valueOf(levels));
        		for (i = levels; i >= 1; i--) {
        			labelComponents[3*(levels-i)+1] = new JLabel("HL" + String.valueOf(i));
        			labelComponents[3*(levels-i)+2] = new JLabel("LH" + String.valueOf(i));
        			labelComponents[3*(levels-i)+3] = new JLabel("HH" + String.valueOf(i));
        		}	
        	} // else if (axesTransformed == 2)
        	else {
        		numComponents = levels + 1;
        		labelComponents = new JLabel[numComponents];
        		labelComponents[0] = new JLabel("L" + String.valueOf(levels));
        		for (i = levels; i >= 1; i--) {
        			labelComponents[(levels-i)+1] = new JLabel("H" + String.valueOf(i));
        		}		
        	}
        } // else if (multiLevelDWTButton.isSelected())
        else { // SWTButton.isSelected()
        	if ((image.getNDims() == 2) || ((image.getNDims() > 2) && (!zAxisCheckBox.isSelected()))) {
        		if (axesTransformed == 2) {
        			numComponents = 4*levels; 
       	            labelComponents = new JLabel[numComponents];
       	            for (i = levels; i >= 1; i--) {
       	                labelComponents[4*(levels-i)] = new JLabel("A" + String.valueOf(i + start_level));
       	                labelComponents[4*(levels-i)+1] = new JLabel("H" + String.valueOf(i + start_level));
       	                labelComponents[4*(levels-i)+2] = new JLabel("V" + String.valueOf(i + start_level));
       	                labelComponents[4*(levels-i)+3] = new JLabel("D" + String.valueOf(i + start_level));	
       	            }
        		} // if (axesTransformed == 2)
        		else {
        			numComponents = 2*levels; 
       	            labelComponents = new JLabel[numComponents];
       	            for (i = levels; i >= 1; i--) {
       	                labelComponents[2*(levels-i)] = new JLabel("A" + String.valueOf(i + start_level));
       	                labelComponents[2*(levels-i)+1] = new JLabel("D" + String.valueOf(i + start_level));	
       	            }	
        		}
        	} // if ((image.getNDims() == 2) || ((image.getNDims() > 2) && (!zAxisCheckBox.isSelected())))
        	else if (axesTransformed == 3) {
        		numComponents = 8*levels; 
   	            labelComponents = new JLabel[numComponents];
   	            for (i = levels; i >= 1; i--) {
   	                labelComponents[8*(levels-i)] = new JLabel("LLL" + String.valueOf(i + start_level));
   	                labelComponents[8*(levels-i)+1] = new JLabel("HLL" + String.valueOf(i + start_level));
   	                labelComponents[8*(levels-i)+2] = new JLabel("LHL" + String.valueOf(i + start_level));
   	                labelComponents[8*(levels-i)+3] = new JLabel("HHL" + String.valueOf(i + start_level));
   	                labelComponents[8*(levels-i)+4] = new JLabel("LLH" + String.valueOf(i + start_level));
	                labelComponents[8*(levels-i)+5] = new JLabel("HLH" + String.valueOf(i + start_level));
	                labelComponents[8*(levels-i)+6] = new JLabel("LHH" + String.valueOf(i + start_level));
	                labelComponents[8*(levels-i)+7] = new JLabel("HHH" + String.valueOf(i + start_level));	
   	            }	
        	} // else if (axesTransformed == 3)
        	else if (axesTransformed == 2) {
        		numComponents = 4*levels; 
   	            labelComponents = new JLabel[numComponents];
   	            for (i = levels; i >= 1; i--) {
   	                labelComponents[4*(levels-i)] = new JLabel("LL" + String.valueOf(i + start_level));
   	                labelComponents[4*(levels-i)+1] = new JLabel("HL" + String.valueOf(i + start_level));
   	                labelComponents[4*(levels-i)+2] = new JLabel("LH" + String.valueOf(i + start_level));
   	                labelComponents[4*(levels-i)+3] = new JLabel("HH" + String.valueOf(i + start_level));
   	            }
        	} // else if (axesTransformed == 2)
        	else {
        		numComponents = 2*levels; 
   	            labelComponents = new JLabel[numComponents];
   	            for (i = levels; i >= 1; i--) {
   	                labelComponents[2*(levels-i)] = new JLabel("L" + String.valueOf(i + start_level));
   	                labelComponents[2*(levels-i)+1] = new JLabel("H" + String.valueOf(i + start_level));
   	            }    	
	        }
        } // else SWTButton.isSelected()
        filterType = new int[numComponents];
        filterVal1 = new double[numComponents];
        filterVal2 = new double[numComponents];
        for (i = 0; i < numComponents; i++) {
        	filterType[i] = FILTER_NONE;
        }
        comboBoxFilterType = new JComboBox[numComponents];
        labelVal1 = new JLabel[numComponents];
        textVal1 = new JTextField[numComponents];
        labelVal2 = new JLabel[numComponents];
        textVal2 = new JTextField[numComponents];
        for (i = 0; i < numComponents; i++) {
        	 labelComponents[i].setFont(serif12);
             labelComponents[i].setForeground(Color.black);
             gbc.gridx = 0;
             gbc.gridy = 3*i;
             filterPanel.add(labelComponents[i], gbc);
             
             comboBoxFilterType[i] = buildFilterTypeComboBox();
             gbc.gridx = 1;
             filterPanel.add(comboBoxFilterType[i],gbc);
             
             labelVal1[i] = new JLabel("Value");
             labelVal1[i].setFont(serif12);
             labelVal1[i].setForeground(Color.black);
             labelVal1[i].setEnabled(false);
             gbc.gridx = 0;
             gbc.gridy = 3*i+1;
             filterPanel.add(labelVal1[i], gbc);
             
             textVal1[i] = new JTextField(10);
             textVal1[i].setFont(serif12);
             textVal1[i].setForeground(Color.black);
             textVal1[i].setEnabled(false);
             gbc.gridx = 1;
             filterPanel.add(textVal1[i], gbc);
             
             labelVal2[i] = new JLabel("Substitute");
             labelVal2[i].setFont(serif12);
             labelVal2[i].setForeground(Color.black);
             labelVal2[i].setEnabled(false);
             gbc.gridx = 0;
             gbc.gridy = 3*i+2;
             filterPanel.add(labelVal2[i], gbc);
             
             textVal2[i] = new JTextField(10);
             textVal2[i].setFont(serif12);
             textVal2[i].setForeground(Color.black);
             textVal2[i].setEnabled(false);
             gbc.gridx = 1;
             filterPanel.add(textVal2[i], gbc);
        }
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
    
    private JComboBox<String> buildFilterTypeComboBox() {
    	final JComboBox<String> comboBox = new JComboBox<String>();
    	comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);
        comboBox.addItem("NONE");
        comboBox.addItem("SOFT");
        comboBox.addItem("NN_GARROTE");
        comboBox.addItem("HARD");
        comboBox.addItem("GREATER");
        comboBox.addItem("LESS");
        comboBox.addItem("THRESHOLD_FIRM");
        comboBox.setSelectedIndex(0);
        comboBox.addActionListener(this);
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
        int index;
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
        	tmpStr = (String)comboBoxLevels.getSelectedItem();
        	levels = Integer.parseInt(tmpStr);
        }
        
        if (tType == SWT) {
        	tmpStr = (String)comboBoxStartLevel.getSelectedItem();
        	start_level = Integer.parseInt(tmpStr);
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
	        index = 0;
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
        
        names = new PyWavelets.WAVELET_NAME[axes.length];
        String nameString[] = new String[axes.length];
        index = 0;
        if (doX) {
        	nameString[index++] = (String) comboBoxNameX.getSelectedItem();
        }
        if (doY) {
        	nameString[index++] = (String) comboBoxNameY.getSelectedItem();
        }
        if (doZ) {
        	nameString[index] = (String) comboBoxNameZ.getSelectedItem();
        }
        for (i = 0; i < axes.length; i++) {
        	if (nameString[i].equals("Biorthogonal")) {
        		names[i] = PyWavelets.WAVELET_NAME.BIOR;
        	}
        	else if (nameString[i].equals("Coiflets")) {
        		names[i] = PyWavelets.WAVELET_NAME.COIF;
        	}
        	else if (nameString[i].equals("Daubechies")) {
        		names[i] = PyWavelets.WAVELET_NAME.DB;
        	}
        	else if (nameString[i].equals("Haar")) {
        		names[i] = PyWavelets.WAVELET_NAME.HAAR;
        	}
        	else if (nameString[i].equals("Reverse biorthogonal")) {
        		names[i] = PyWavelets.WAVELET_NAME.RBIO;
        	}
        	else if (nameString[i].equals("Symlets")) {
        		names[i] = PyWavelets.WAVELET_NAME.SYM;
        	}
        }
        
        orders = new int[axes.length];
        index = 0;
        if (doX) {
        	tmpStr = (String) comboBoxOrderX.getSelectedItem();
        	orders[index++] = Integer.valueOf(tmpStr).intValue();
        }
        if (doY) {
        	tmpStr = (String) comboBoxOrderY.getSelectedItem();
        	orders[index++] = Integer.valueOf(tmpStr).intValue();
        }
        if (doZ) {
        	tmpStr = (String) comboBoxOrderZ.getSelectedItem();
        	orders[index++] = Integer.valueOf(tmpStr).intValue();
        }
        
        for (i = 0; i < numComponents; i++) {
        	String selection = (String)comboBoxFilterType[i].getSelectedItem();
        	if (selection.equals("NONE")) {
        		filterType[i] = FILTER_NONE;
        	}
        	else if (selection.equals("SOFT")) {
        		filterType[i] = FILTER_SOFT;
        	}
        	else if (selection.equals("NN_GARROTE")) {
        		filterType[i] = FILTER_NN_GARROTE;
        	}
        	else if (selection.equals("HARD")) {
        	    filterType[i] = FILTER_HARD;
        	}
        	else if (selection.equals("GREATER")) {
        	    filterType[i] = FILTER_GREATER;
        	}
        	else if (selection.equals("LESS")) {
        	    filterType[i] = FILTER_LESS;
        	}
        	else if (selection.equals("THRESHOLD_FIRM")) {
        	    filterType[i] = FILTER_THRESHOLD_FIRM;
        	}
        	
        	if (filterType[i] != FILTER_NONE) {
        		tmpStr = textVal1[i].getText();	
        		try {
        			filterVal1[i] = Double.valueOf(tmpStr).doubleValue();
        		}
        		catch (NumberFormatException e) {
        		    MipavUtil.displayError("textval1["+i+"] does not have a proper number");
        		    textVal1[i].requestFocus();
        		    textVal1[i].selectAll();
        		    return false;
        		}
        		
        		tmpStr = textVal2[i].getText();	
        		try {
        			filterVal2[i] = Double.valueOf(tmpStr).doubleValue();
        		}
        		catch (NumberFormatException e) {
        		    MipavUtil.displayError("textval2["+i+"] does not have a proper number");
        		    textVal2[i].requestFocus();
        		    textVal2[i].selectAll();
        		    return false;
        		}
        	} // if (filterType[i] != FILTER_NONE)
        	
        	if (filterType[i] == FILTER_THRESHOLD_FIRM) {
        		if (filterVal1[i] < 0.0) {
        			MipavUtil.displayError("filterVal1["+i+"] must be >= 0");
        			textVal1[i].requestFocus();
        		    textVal1[i].selectAll();
        		    return false;
        		}
        		if (filterVal2[i] < filterVal1[i]) {
        			MipavUtil.displayError("filterVal2["+i+"] must be >= filterVal1["+i+"]");
        			textVal2[i].requestFocus();
        		    textVal2[i].selectAll();
        		    return false;
        		}
        	}
        } // for (i = 0; i < numComponents; i++)
        
        doBayesShrinkThresholdComputation = BayesCheckBox.isSelected();
        return true;
    }
}
