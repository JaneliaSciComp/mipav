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
	
	private JTextField baseText;
	
	private JCheckBox spatialSmoothingCheckBox;
	
	private boolean spatialSmoothing = false;
	
	private JLabel sigmaXLabel;
	
	private JTextField sigmaXText;
	
	private float sigmax = 5.0f;
	
    private JLabel sigmaYLabel;
	
	private JTextField sigmaYText;
	
	private float sigmay = 5.0f;
	
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
	
	private JLabel edgeLabel;
	
	private JTextField edgeText;
	
	private float edgeKernelSize = 0.50f;
	
	private JRadioButton autoButton;
	
	private JRadioButton DSCMRIToolboxButton;
	
	private JRadioButton autoRAPIDButton;
	
	private JRadioButton pickButton;
	
	private boolean autoAIFCalculation = true;
	
	private boolean findAIFInfoWithDSCMRIToolbox = false;
	
	private JLabel lowZSliceLabel;
	
	private JTextField lowZSliceText;
	
	private int selectedAIFLowZSlice = -1;
	
    private JLabel highZSliceLabel;
	
	private JTextField highZSliceText;
	
	private int selectedAIFHighZSlice = -1;
	
	private JCheckBox plotAIFCheckBox;
	
	private boolean plotAIF = true;
	
	private JCheckBox multiThreadingEnabledCheckBox;
	
	private boolean multiThreading;
	
	private ButtonGroup searchGroup;
	
	private JRadioButton search1DElsuncButton;
	
	private JRadioButton search2DElsuncButton;
	
	private JRadioButton search2DNMSimplexButton;
	
	private JRadioButton search2DNelderMeadButton;
	
	private int search = PlugInAlgorithmTSPAnalysis.ELSUNC_2D_SEARCH;
	
	private JCheckBox correlationCheckBox;
	
	private boolean calculateCorrelation = true;
	
	private JCheckBox CBFCBVMTTCheckBox;
	
	private boolean calculateCBFCBVMTT = true;
	
	private JCheckBox boundsCheckBox;
	
	private boolean calculateBounds = false;
	
	private String fileNameBase = "IM";
	
	private boolean experimentalRAPIDAIF = false;
	
	private JCheckBox N4CheckBox;
	
	private boolean doN4MRIBiasFieldCorrection = false;
	
	private JCheckBox saveOriginalCheckBox;
	
	private boolean saveOriginalData = false;
	
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
        } else if((source == search1DElsuncButton) || (source == search2DElsuncButton) ||
        		(source == search2DNMSimplexButton) || (source == search2DNelderMeadButton)) {
        	if (search2DElsuncButton.isSelected() || (source == search2DNMSimplexButton) || (source == search2DNelderMeadButton)) {
        		boundsCheckBox.setEnabled(true);
        	}
        	else {
        		boundsCheckBox.setEnabled(false);
        		boundsCheckBox.setSelected(false);
        	}
        } else if (source == spatialSmoothingCheckBox) {
        	sigmaXLabel.setEnabled(spatialSmoothingCheckBox.isSelected());
        	sigmaXText.setEnabled(spatialSmoothingCheckBox.isSelected());
        	sigmaYLabel.setEnabled(spatialSmoothingCheckBox.isSelected());
        	sigmaYText.setEnabled(spatialSmoothingCheckBox.isSelected());
        } else if ((source == autoButton) || (source == autoRAPIDButton) || (source == pickButton) || (source == DSCMRIToolboxButton)) {
        	edgeLabel.setEnabled(autoRAPIDButton.isSelected());
        	edgeText.setEnabled(autoRAPIDButton.isSelected());
        	lowZSliceLabel.setEnabled(DSCMRIToolboxButton.isSelected());
        	lowZSliceText.setEnabled(DSCMRIToolboxButton.isSelected());
        	highZSliceLabel.setEnabled(DSCMRIToolboxButton.isSelected());
        	highZSliceText.setEnabled(DSCMRIToolboxButton.isSelected());
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
        
        pwiImageFileDirectoryText = gui.buildFileField("Directory containing pwi image file: ", "", false, JFileChooser.DIRECTORIES_ONLY);
        inputPanel.add(pwiImageFileDirectoryText.getParent(), gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        JLabel baseLabel = new JLabel("Base name in selected files");
        baseLabel.setFont(serif12);
        baseLabel.setForeground(Color.black);
        inputPanel.add(baseLabel, gbc);
        
        gbc.gridx = 1;
        baseText = new JTextField(10);
        baseText.setText("IM");
        baseText.setFont(serif12);
        baseText.setForeground(Color.black);
        inputPanel.add(baseText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        N4CheckBox = new JCheckBox("Perform N4 MRI Bias Field Correction");
        N4CheckBox.setSelected(false);
        N4CheckBox.setFont(MipavUtil.font12);
        N4CheckBox.setForeground(Color.black);
        N4CheckBox.addActionListener(this);
        inputPanel.add(N4CheckBox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        spatialSmoothingCheckBox = new JCheckBox("Perform spatial smoothing");
        spatialSmoothingCheckBox.setSelected(false);
        spatialSmoothingCheckBox.setFont(MipavUtil.font12);
        spatialSmoothingCheckBox.setForeground(Color.black);
        spatialSmoothingCheckBox.addActionListener(this);
        inputPanel.add(spatialSmoothingCheckBox, gbc);
        
        gbc.gridy++;
        sigmaXLabel = new JLabel("Gaussian blur sigma X in millimeters");
        sigmaXLabel.setFont(serif12);
        sigmaXLabel.setForeground(Color.black);
        sigmaXLabel.setEnabled(false);
        inputPanel.add(sigmaXLabel, gbc);
        
        gbc.gridx = 1;
        sigmaXText = new JTextField(10);
        sigmaXText.setText("5.0");
        sigmaXText.setFont(serif12);
        sigmaXText.setForeground(Color.black);
        sigmaXText.setEnabled(false);
        inputPanel.add(sigmaXText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        sigmaYLabel = new JLabel("Gaussian blur sigma Y in millimeters");
        sigmaYLabel.setFont(serif12);
        sigmaYLabel.setForeground(Color.black);
        sigmaYLabel.setEnabled(false);
        inputPanel.add(sigmaYLabel, gbc);
        
        gbc.gridx = 1;
        sigmaYText = new JTextField(10);
        sigmaYText.setText("5.0");
        sigmaYText.setFont(serif12);
        sigmaYText.setForeground(Color.black);
        sigmaYText.setEnabled(false);
        inputPanel.add(sigmaYText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        calculateMaskingCheckBox = new JCheckBox("Calculate masking threshold from mean and standard deviation");
        calculateMaskingCheckBox.setSelected(true);
        calculateMaskingCheckBox.setFont(MipavUtil.font12);
        calculateMaskingCheckBox.setForeground(Color.black);
        calculateMaskingCheckBox.addActionListener(this);
        inputPanel.add(calculateMaskingCheckBox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
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
        gbc.gridy++;
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
        gbc.gridy++;
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
        gbc.gridy++;
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
        gbc.gridy++;
        autoButton = new JRadioButton("Auto AIF Calculation", true);
        autoButton.setFont(serif12);
        autoButton.setForeground(Color.black);
        AIFGroup.add(autoButton);
        autoButton.addActionListener(this);
        inputPanel.add(autoButton, gbc);
        
        gbc.gridy++;
        DSCMRIToolboxButton = new JRadioButton("Obtain AIF from DSC_MRI_Toolbox", false);
        DSCMRIToolboxButton.setFont(serif12);
        DSCMRIToolboxButton.setForeground(Color.black);
        AIFGroup.add(DSCMRIToolboxButton);
        DSCMRIToolboxButton.addActionListener(this);
        inputPanel.add(DSCMRIToolboxButton, gbc);
        
        gbc.gridy++;
        lowZSliceLabel = new JLabel("Selected low Z slice for AIF (0 to zDim-1)");
        lowZSliceLabel.setFont(serif12);
        lowZSliceLabel.setForeground(Color.black);
        lowZSliceLabel.setEnabled(false);
        inputPanel.add(lowZSliceLabel, gbc);
        
        gbc.gridx = 1;
        lowZSliceText = new JTextField(10);
        lowZSliceText.setText("4");
        lowZSliceText.setFont(serif12);
        lowZSliceText.setForeground(Color.black);
        lowZSliceText.setEnabled(false);
        inputPanel.add(lowZSliceText, gbc);  
        
        gbc.gridx = 0;
        gbc.gridy++;
        highZSliceLabel = new JLabel("Selected high Z slice for AIF (0 to zDim-1)");
        highZSliceLabel.setFont(serif12);
        highZSliceLabel.setForeground(Color.black);
        highZSliceLabel.setEnabled(false);
        inputPanel.add(highZSliceLabel, gbc);
        
        gbc.gridx = 1;
        highZSliceText = new JTextField(10);
        highZSliceText.setText("8");
        highZSliceText.setFont(serif12);
        highZSliceText.setForeground(Color.black);
        highZSliceText.setEnabled(false);
        inputPanel.add(highZSliceText, gbc);  
        
        gbc.gridx = 0;
        gbc.gridy++;
        autoRAPIDButton = new JRadioButton("Experiemental RAPID auto AIF Calculation", false);
        autoRAPIDButton.setFont(serif12);
        autoRAPIDButton.setForeground(Color.black);
        AIFGroup.add(autoRAPIDButton);
        autoRAPIDButton.addActionListener(this);
        inputPanel.add(autoRAPIDButton, gbc);
        
        gbc.gridy++;
        edgeLabel = new JLabel("Brain surface extractor edge kernel size");
        edgeLabel.setFont(serif12);
        edgeLabel.setForeground(Color.black);
        edgeLabel.setEnabled(false);
        inputPanel.add(edgeLabel, gbc);
        
        gbc.gridx = 1;
        edgeText = new JTextField(10);
        edgeText.setText("0.5");
        edgeText.setFont(serif12);
        edgeText.setForeground(Color.black);
        edgeText.setEnabled(false);
        inputPanel.add(edgeText, gbc);   
        
        gbc.gridx = 0;
        gbc.gridy++;
        pickButton = new JRadioButton("Pick image pixel corresponding to AIF", false);
        pickButton.setFont(serif12);
        pickButton.setForeground(Color.black);
        AIFGroup.add(pickButton);
        pickButton.addActionListener(this);
        inputPanel.add(pickButton, gbc);
        
        gbc.gridy++;
        plotAIFCheckBox = new JCheckBox("Plot AIF");
        plotAIFCheckBox.setFont(MipavUtil.font12);
        plotAIFCheckBox.setSelected(true);
        inputPanel.add(plotAIFCheckBox, gbc);
        
        gbc.gridy++;
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
        
        searchGroup = new ButtonGroup();
        gbc.gridx = 0;
        gbc.gridy++;
        search2DElsuncButton = new JRadioButton("2D Elsunc search", true);
        search2DElsuncButton.setFont(serif12);
        search2DElsuncButton.setForeground(Color.black);
        search2DElsuncButton.addActionListener(this);
        searchGroup.add(search2DElsuncButton);
        inputPanel.add(search2DElsuncButton, gbc);   
        
        gbc.gridy++;
        search1DElsuncButton = new JRadioButton("1D Elsunc search", false);
        search1DElsuncButton.setFont(serif12);
        search1DElsuncButton.setForeground(Color.black);
        search1DElsuncButton.addActionListener(this);
        searchGroup.add(search1DElsuncButton);
        inputPanel.add(search1DElsuncButton, gbc);
        
        gbc.gridy++;
        search2DNMSimplexButton = new JRadioButton("2D Michael Hutt NMSimplex search", false);
        search2DNMSimplexButton.setFont(serif12);
        search2DNMSimplexButton.setForeground(Color.black);
        search2DNMSimplexButton.addActionListener(this);
        searchGroup.add(search2DNMSimplexButton);
        inputPanel.add(search2DNMSimplexButton, gbc);
        
        gbc.gridy++;
        search2DNelderMeadButton = new JRadioButton("2D Matteo Magioni NelderMead search", false);
        search2DNelderMeadButton.setFont(serif12);
        search2DNelderMeadButton.setForeground(Color.black);
        search2DNelderMeadButton.addActionListener(this);
        searchGroup.add(search2DNelderMeadButton);
        inputPanel.add(search2DNelderMeadButton, gbc);
        
        gbc.gridy++;
        correlationCheckBox = new JCheckBox("Correlation maps calculated", true);
        correlationCheckBox.setFont(MipavUtil.font12);
        correlationCheckBox.setForeground(Color.black);
        inputPanel.add(correlationCheckBox, gbc);
        
        gbc.gridy++;
        CBFCBVMTTCheckBox = new JCheckBox("CBF, CBV, and MTT calculated", true);
        CBFCBVMTTCheckBox.setFont(MipavUtil.font12);
        CBFCBVMTTCheckBox.setForeground(Color.black);
        inputPanel.add(CBFCBVMTTCheckBox, gbc);
        
        gbc.gridy++;
        boundsCheckBox = new JCheckBox("Bounds calculation enabled", false);
        boundsCheckBox.setFont(MipavUtil.font12);
        boundsCheckBox.setForeground(Color.black);
        inputPanel.add(boundsCheckBox, gbc);
        
        gbc.gridy++;
        saveOriginalCheckBox = new JCheckBox("Save original data and exit" , false);
        saveOriginalCheckBox.setFont(MipavUtil.font12);
        saveOriginalCheckBox.setForeground(Color.black);
        inputPanel.add(saveOriginalCheckBox, gbc);
        
        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(inputPanel, BorderLayout.NORTH);
        mainPanel.add(OKCancelPanel, BorderLayout.SOUTH);
        
        JScrollPane scrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        
        getContentPane().add(scrollPane);

        pack();
        setVisible(true);
        setResizable(true);
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

            TSPAnalysisAlgo = new PlugInAlgorithmTSPAnalysis(pwiImageFileDirectory, spatialSmoothing, sigmax,
            		sigmay, calculateMaskingThreshold, masking_threshold,
            		TSP_threshold, TSP_iter, Psvd, autoAIFCalculation, plotAIF, multiThreading, search, calculateCorrelation,
            		calculateCBFCBVMTT, calculateBounds, fileNameBase, findAIFInfoWithDSCMRIToolbox, selectedAIFLowZSlice,
            		selectedAIFHighZSlice, experimentalRAPIDAIF, edgeKernelSize, doN4MRIBiasFieldCorrection,
            		saveOriginalData);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            TSPAnalysisAlgo.addListener(this);
            createProgressBar("Creating plugin", " ...", TSPAnalysisAlgo);
            progressBar.setVisible(true);

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
    	doN4MRIBiasFieldCorrection = scriptParameters.getParams().getBoolean("doN4");
    	spatialSmoothing = scriptParameters.getParams().getBoolean("spatial_smoothing");
    	sigmax = scriptParameters.getParams().getFloat("x_sigma");
    	sigmay = scriptParameters.getParams().getFloat("y_sigma");
    	calculateMaskingThreshold = scriptParameters.getParams().getBoolean("calc_mask_thresh");
    	masking_threshold = scriptParameters.getParams().getInt("mask_thresh");
    	TSP_threshold = scriptParameters.getParams().getDouble("TSP_thresh");
    	TSP_iter = scriptParameters.getParams().getInt("iter");
    	Psvd = scriptParameters.getParams().getDouble("psv");
    	autoAIFCalculation = scriptParameters.getParams().getBoolean("auto_AIF");
    	plotAIF = scriptParameters.getParams().getBoolean("plot_AIF");
    	multiThreading = scriptParameters.getParams().getBoolean("multi_thread");
    	search = scriptParameters.getParams().getInt("search_pars");
    	calculateCorrelation = scriptParameters.getParams().getBoolean("calc_correlation");
    	calculateCBFCBVMTT = scriptParameters.getParams().getBoolean("calc_CBFCBVMTT");
    	calculateBounds = scriptParameters.getParams().getBoolean("calc_bounds");
    	findAIFInfoWithDSCMRIToolbox = scriptParameters.getParams().getBoolean("AIF_DSC_MRI_Toolbox");
    	selectedAIFLowZSlice = scriptParameters.getParams().getInt("low_z_slice");
    	selectedAIFHighZSlice = scriptParameters.getParams().getInt("high_z_slice");
    	experimentalRAPIDAIF = scriptParameters.getParams().getBoolean("experimental_aif");
    	edgeKernelSize = scriptParameters.getParams().getFloat("edge_kernel");
    	saveOriginalData = scriptParameters.getParams().getBoolean("save_original");
    }
    
    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.getParams().put(ParameterFactory.newParameter("pwiImageFileDirectory", pwiImageFileDirectory));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("doN4", doN4MRIBiasFieldCorrection));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("spatial_smoothing", spatialSmoothing));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("x_sigma", sigmax));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("y_sigma", sigmay));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("calc_mask_thresh", calculateMaskingThreshold));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("mask_thresh", masking_threshold));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("TSP_thresh", TSP_threshold));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("iter", TSP_iter));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("psv", Psvd));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("auto_AIF", autoAIFCalculation));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("plot_AIF", plotAIF));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("multi_thread", multiThreading));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("search_pars", search));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("calc_correlation", calculateCorrelation));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("calc_CBFCBVMTT", calculateCBFCBVMTT));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("calc_bounds", calculateBounds));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("AIF_DSC_MRI_Toolbox", findAIFInfoWithDSCMRIToolbox));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("low_z_slice", selectedAIFLowZSlice));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("high_z_slice", selectedAIFHighZSlice));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("experimental_aif", experimentalRAPIDAIF));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("edge_kernel", edgeKernelSize));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("save_original", saveOriginalData));
    }
    
    private boolean setVariables() {
    	String tmpStr;
    	pwiImageFileDirectory = pwiImageFileDirectoryText.getText();
    	
    	fileNameBase = baseText.getText().trim();
    	if (fileNameBase == null) {
    		MipavUtil.displayError("The file name base is empty");
    		baseText.requestFocus();
    		baseText.selectAll();
    		return false;
    	}
    	
    	doN4MRIBiasFieldCorrection = N4CheckBox.isSelected();
    	
    	spatialSmoothing = spatialSmoothingCheckBox.isSelected();
    	if (spatialSmoothing) {
	    	tmpStr = sigmaXText.getText();
	    	try {
	    		sigmax = Float.valueOf(tmpStr).floatValue();
	    	}
	    	catch (NumberFormatException e) {
	    	    MipavUtil.displayError("sigmax text does not have a proper float");
	    	    sigmaXText.requestFocus();
	    	    sigmaXText.selectAll();
	    	    return false;
	    	}
	    	if (sigmax < 0) {
	    		MipavUtil.displayError("sigmax must be at least 0");
	    		sigmaXText.requestFocus();
	    	    sigmaXText.selectAll();
	    	    return false;
	    	}
	    	
	    	tmpStr = sigmaYText.getText();
	    	try {
	    		sigmay = Float.valueOf(tmpStr).floatValue();
	    	}
	    	catch (NumberFormatException e) {
	    	    MipavUtil.displayError("sigmay text does not have a proper float");
	    	    sigmaYText.requestFocus();
	    	    sigmaYText.selectAll();
	    	    return false;
	    	}
	    	if (sigmay < 0) {
	    		MipavUtil.displayError("sigmay must be at least 0");
	    		sigmaYText.requestFocus();
	    	    sigmaYText.selectAll();
	    	    return false;
	    	}

    	} // if (spatialSmoothing)
    	
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
    	findAIFInfoWithDSCMRIToolbox = DSCMRIToolboxButton.isSelected();
    	if (findAIFInfoWithDSCMRIToolbox) {
    	    tmpStr = lowZSliceText.getText();
    	    try {
    	    	selectedAIFLowZSlice = Integer.valueOf(tmpStr).intValue();
    	    }
    	    catch (NumberFormatException e) {
        	    MipavUtil.displayError("Selected AIF low Z slice does not have a proper integer number");
        	    lowZSliceText.requestFocus();
        	    lowZSliceText.selectAll();
        	    return false;
        	}
        	if (selectedAIFLowZSlice < 0) {
        		MipavUtil.displayError("Selected AIF low Z slice must be >= 0");
        		lowZSliceText.requestFocus();
        	    lowZSliceText.selectAll();
        	    return false;
        	}
        	
        	tmpStr = highZSliceText.getText();
    	    try {
    	    	selectedAIFHighZSlice = Integer.valueOf(tmpStr).intValue();
    	    }
    	    catch (NumberFormatException e) {
        	    MipavUtil.displayError("Selected AIF high Z slice does not have a proper integer number");
        	    highZSliceText.requestFocus();
        	    highZSliceText.selectAll();
        	    return false;
        	}
        	if (selectedAIFHighZSlice < selectedAIFLowZSlice) {
        		MipavUtil.displayError("Selected AIF high Z slice must be >= selectedAIFLowZSlice");
        		highZSliceText.requestFocus();
        	    highZSliceText.selectAll();
        	    return false;
        	}	
    	} // if (findAIFInfoWithDSCMRIToolbox)
    	
    	experimentalRAPIDAIF = autoRAPIDButton.isSelected();
    	if (experimentalRAPIDAIF) {
    		tmpStr = edgeText.getText();
        	try {
        		edgeKernelSize = Float.valueOf(tmpStr).floatValue();
        	}
        	catch (NumberFormatException e) {
        	    MipavUtil.displayError("Edge kernel text does not have a proper float number");
        	    edgeText.requestFocus();
        	    edgeText.selectAll();
        	    return false;
        	}
        	if (edgeKernelSize <= 0) {
        		MipavUtil.displayError("Edge kernel size must be at greater than 0");
        		edgeText.requestFocus();
        	    edgeText.selectAll();
        	    return false;
        	}	
    	}
    	plotAIF = plotAIFCheckBox.isSelected();
    	multiThreading = multiThreadingEnabledCheckBox.isSelected();
    	if (search2DElsuncButton.isSelected()) {
    		search = PlugInAlgorithmTSPAnalysis.ELSUNC_2D_SEARCH;
    	}
    	else if (search1DElsuncButton.isSelected()) {
    	    search = PlugInAlgorithmTSPAnalysis.ELSUNC_1D_SEARCH;	
    	}
    	else if (search2DNMSimplexButton.isSelected()) {
    		search = PlugInAlgorithmTSPAnalysis.NMSIMPLEX_2D_SEARCH;
    	}
    	else if (search2DNelderMeadButton.isSelected()) {
    		search = PlugInAlgorithmTSPAnalysis.NELDERMEAD_2D_SEARCH;
    	}
    	
    	calculateCorrelation = correlationCheckBox.isSelected();
    	
    	calculateCBFCBVMTT = CBFCBVMTTCheckBox.isSelected();
    	
    	calculateBounds = boundsCheckBox.isSelected();
    	
    	saveOriginalData = saveOriginalCheckBox.isSelected();
    	
    	return true;
    }

}
