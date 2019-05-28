import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 
 */
public class PlugInDialogTSPAnalysis extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	private PlugInAlgorithmTSPAnalysis TSPAnalysisAlgo = null;
	
	private JTextField pwiImageFileDirectoryText;

	private String pwiImageFileDirectory;
	
	private JCheckBox calculateMaskingCheckBox;
	
	private boolean calculateMaskingThreshold = true;
	
	private JLabel masking_thresholdLabel;
	
	private JTextField masking_thresholdText;
	
	// Threshold to mask out image pixels not corresponding to brain tissues
	private int masking_threshold = 600;
	
	private JTextField TSP_thresholdText;
	
	private double TSP_threshold = 0.8;
	
    private JTextField TSP_iterText;
	
	private int TSP_iter = 4;
	
	private JTextField PsvdText;
	
	private double Psvd = 0.1;
	
	private ButtonGroup AIFGroup;
	
	private JRadioButton autoButton;
	
	private JRadioButton pickButton;
	
	private boolean autoAIFCalculation = true;
	
	private JCheckBox multiThreadingEnabledCheckBox;
	
	private boolean multiThreading;
	
	//private ButtonGroup searchGroup;
	
	//private JRadioButton search1DButton;
	
	//private JRadioButton search2DButton;
	
	private boolean search2D = true;
	
	/**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogTSPAnalysis() {}

    /**
     * 
     */
    public PlugInDialogTSPAnalysis(final boolean modal) {
        super(modal);

        init();
    }
    
 // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event Event that triggers function.
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == calculateMaskingCheckBox) {
             masking_thresholdLabel.setEnabled(!calculateMaskingCheckBox.isSelected());
        	 masking_thresholdText.setEnabled(!calculateMaskingCheckBox.isSelected());	
        } else {
            super.actionPerformed(event);
        }
        // System.out.print(this.getSize());
    } // end actionPerformed()
	
	/**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("TSP Analysis");
        
        final GuiBuilder gui = new GuiBuilder(this);

        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);
        inputPanel.setBorder(buildTitledBorder("Input parameters"));
        
        pwiImageFileDirectoryText = gui.buildFileField("Directory containing pwi image file: ", " ", false, JFileChooser.DIRECTORIES_ONLY);
        inputPanel.add(pwiImageFileDirectoryText.getParent(), gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        calculateMaskingCheckBox = new JCheckBox("Calculate masking threshold from mean and standard deviation");
        calculateMaskingCheckBox.setSelected(true);
        calculateMaskingCheckBox.setFont(MipavUtil.font12);
        calculateMaskingCheckBox.setForeground(Color.black);
        calculateMaskingCheckBox.addActionListener(this);
        inputPanel.add(calculateMaskingCheckBox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        masking_thresholdLabel = new JLabel("Masking threshold");
        masking_thresholdLabel.setFont(serif12);
        masking_thresholdLabel.setForeground(Color.black);
        masking_thresholdLabel.setEnabled(false);
        inputPanel.add(masking_thresholdLabel, gbc);
        
        gbc.gridx = 1;
        masking_thresholdText = new JTextField(10);
        masking_thresholdText.setText("600");
        masking_thresholdText.setFont(serif12);
        masking_thresholdText.setForeground(Color.black);
        masking_thresholdText.setEnabled(false);
        inputPanel.add(masking_thresholdText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 3;
        JLabel TSP_thresholdLabel = new JLabel("TSP threshold");
        TSP_thresholdLabel.setFont(serif12);
        TSP_thresholdLabel.setForeground(Color.black);
        inputPanel.add(TSP_thresholdLabel, gbc);
        
        gbc.gridx = 1;
        TSP_thresholdText = new JTextField(10);
        TSP_thresholdText.setText("0.8");
        TSP_thresholdText.setFont(serif12);
        TSP_thresholdText.setForeground(Color.black);
        inputPanel.add(TSP_thresholdText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 4;
        JLabel TSP_iterLabel = new JLabel("TSP iterations");
        TSP_iterLabel.setFont(serif12);
        TSP_iterLabel.setForeground(Color.black);
        inputPanel.add(TSP_iterLabel, gbc);
        
        gbc.gridx = 1;
        TSP_iterText = new JTextField(10);
        TSP_iterText.setText("4");
        TSP_iterText.setFont(serif12);
        TSP_iterText.setForeground(Color.black);
        inputPanel.add(TSP_iterText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 5;
        JLabel PsvdLabel = new JLabel("Psvd");
        PsvdLabel.setFont(serif12);
        PsvdLabel.setForeground(Color.black);
        inputPanel.add(PsvdLabel, gbc);
        
        gbc.gridx = 1;
        PsvdText = new JTextField(10);
        PsvdText.setText("0.1");
        PsvdText.setFont(serif12);
        PsvdText.setForeground(Color.black);
        inputPanel.add(PsvdText, gbc);
        
        AIFGroup = new ButtonGroup();
        gbc.gridx = 0;
        gbc.gridy = 6;
        autoButton = new JRadioButton("Auto AIF Calculation", true);
        autoButton.setFont(serif12);
        autoButton.setForeground(Color.black);
        AIFGroup.add(autoButton);
        inputPanel.add(autoButton, gbc);
        
        gbc.gridy = 7;
        pickButton = new JRadioButton("Pick image pixel corresponding to AIF", false);
        pickButton.setFont(serif12);
        pickButton.setForeground(Color.black);
        AIFGroup.add(pickButton);
        inputPanel.add(pickButton, gbc);
        
        gbc.gridy = 8;
        if (ThreadUtil.getAvailableCores() > 1) {
            multiThreadingEnabledCheckBox = new JCheckBox("Multi-threading enabled (" + ThreadUtil.getAvailableCores() + " cores)");
        }
        else {
        	multiThreadingEnabledCheckBox = new JCheckBox("Multi-threading disabled (" + ThreadUtil.getAvailableCores() + " core)");
        }
        multiThreadingEnabledCheckBox.setFont(MipavUtil.font12);
        multiThreadingEnabledCheckBox.setForeground(Color.black);
        multiThreadingEnabledCheckBox.setSelected(ThreadUtil.getAvailableCores() > 1);
        multiThreadingEnabledCheckBox.setEnabled(ThreadUtil.getAvailableCores() > 1);
        inputPanel.add(multiThreadingEnabledCheckBox, gbc);
        
        /*searchGroup = new ButtonGroup();
        gbc.gridx = 0;
        gbc.gridy = 9;
        search2DButton = new JRadioButton("2D search", true);
        search2DButton.setFont(serif12);
        search2DButton.setForeground(Color.black);
        searchGroup.add(search2DButton);
        inputPanel.add(search2DButton, gbc);
        
        gbc.gridy = 10;
        search1DButton = new JRadioButton("1D search", false);
        search1DButton.setFont(serif12);
        search1DButton.setForeground(Color.black);
        searchGroup.add(search1DButton);
        inputPanel.add(search1DButton, gbc);*/
        
        
        
        getContentPane().add(inputPanel, BorderLayout.NORTH);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
    }

	/**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if (algorithm instanceof PlugInAlgorithmTSPAnalysis) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            //System.out.println("Elapsed: " + algorithm.getElapsedTime());

            if ( (TSPAnalysisAlgo.isCompleted() == true)) {
                insertScriptLine();
            }

            if (TSPAnalysisAlgo != null) {
                TSPAnalysisAlgo.finalize();
                TSPAnalysisAlgo = null;
            }

            // dispose();
            if (super.isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            }
        }
    } // end algorithmPerformed()
    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    @Override
    protected void callAlgorithm() {

        try {

            TSPAnalysisAlgo = new PlugInAlgorithmTSPAnalysis(pwiImageFileDirectory, calculateMaskingThreshold, masking_threshold,
            		TSP_threshold, TSP_iter, Psvd, autoAIFCalculation, multiThreading, search2D);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            TSPAnalysisAlgo.addListener(this);
            // createProgressBar("Creating plugin", " ...", generateFusionAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (TSPAnalysisAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                TSPAnalysisAlgo.run();
            }
        } catch (final OutOfMemoryError x) {
            /*if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }*/

            MipavUtil.displayError("Generic algorithm: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()
    
    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void setGUIFromParams() {
    	pwiImageFileDirectory = scriptParameters.getParams().getString("pwiImageFileDirectory");
    	calculateMaskingThreshold = scriptParameters.getParams().getBoolean("calc_mask_thresh");
    	masking_threshold = scriptParameters.getParams().getInt("mask_thresh");
    	TSP_threshold = scriptParameters.getParams().getDouble("TSP_thresh");
    	TSP_iter = scriptParameters.getParams().getInt("iter");
    	Psvd = scriptParameters.getParams().getDouble("psv");
    	autoAIFCalculation = scriptParameters.getParams().getBoolean("auto_AIF");
    	multiThreading = scriptParameters.getParams().getBoolean("multi_thread");
    	search2D = scriptParameters.getParams().getBoolean("search_2D");
    }
    
    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.getParams().put(ParameterFactory.newParameter("pwiImageFileDirectory", pwiImageFileDirectory));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("calc_mask_thresh", calculateMaskingThreshold));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("mask_thresh", masking_threshold));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("TSP_thresh", TSP_threshold));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("iter", TSP_iter));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("psv", Psvd));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("auto_AIF", autoAIFCalculation));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("multi_thread", multiThreading));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("search_2D", search2D));
    }
    
    private boolean setVariables() {
    	String tmpStr;
    	pwiImageFileDirectory = pwiImageFileDirectoryText.getText();
    	
    	calculateMaskingThreshold = calculateMaskingCheckBox.isSelected();
    	if (!calculateMaskingThreshold) {
	    	tmpStr = masking_thresholdText.getText();
	    	try {
	    		masking_threshold = Integer.valueOf(tmpStr).intValue();
	    	}
	    	catch (NumberFormatException e) {
	    	    MipavUtil.displayError("masking_threshold text does not have a proper integer");
	    	    masking_thresholdText.requestFocus();
	    	    masking_thresholdText.selectAll();
	    	    return false;
	    	}
	    	if (masking_threshold < 0) {
	    		MipavUtil.displayError("masking_threshold must be at least 0");
	    		masking_thresholdText.requestFocus();
	    	    masking_thresholdText.selectAll();
	    	    return false;
	    	}
	    	if (masking_threshold > 65535) {
	    		MipavUtil.displayError("masking_threshold cannot exceed 65535");
	    		masking_thresholdText.requestFocus();
	    	    masking_thresholdText.selectAll();
	    	    return false;
	    	}
    	}
    	
    	tmpStr = TSP_thresholdText.getText();
    	try {
    		TSP_threshold = Double.valueOf(tmpStr).doubleValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("masking_threshold text does not have a proper double number");
    	    TSP_thresholdText.requestFocus();
    	    TSP_thresholdText.selectAll();
    	    return false;
    	}
    	if (TSP_threshold < 0) {
    		MipavUtil.displayError("TSP_threshold must be at least 0");
    		TSP_thresholdText.requestFocus();
    	    TSP_thresholdText.selectAll();
    	    return false;
    	}
    	
    	tmpStr = TSP_iterText.getText();
    	try {
    		TSP_iter = Integer.valueOf(tmpStr).intValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("TSP_iter text does not have a proper integer");
    	    TSP_iterText.requestFocus();
    	    TSP_iterText.selectAll();
    	    return false;
    	}
    	if (TSP_iter < 1) {
    		MipavUtil.displayError("TSP_iter must be at least 1");
    		TSP_iterText.requestFocus();
    	    TSP_iterText.selectAll();
    	    return false;
    	}
    	if (TSP_iter > 100) {
    		MipavUtil.displayError("TSP_iter cannot exceed 100");
    		TSP_iterText.requestFocus();
    	    TSP_iterText.selectAll();
    	    return false;
    	}
    	
    	tmpStr = PsvdText.getText();
    	try {
    		Psvd = Double.valueOf(tmpStr).doubleValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("Psvd text does not have a proper double number");
    	    PsvdText.requestFocus();
    	    PsvdText.selectAll();
    	    return false;
    	}
    	if (Psvd < 0) {
    		MipavUtil.displayError("Psvd must be at least 0");
    		PsvdText.requestFocus();
    	    PsvdText.selectAll();
    	    return false;
    	}
    	
    	autoAIFCalculation = autoButton.isSelected();
    	multiThreading = multiThreadingEnabledCheckBox.isSelected();
    	//search2D = search2DButton.isSelected();
    	return true;
    }

}
