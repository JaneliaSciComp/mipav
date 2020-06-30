import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;

import javax.swing.*;


/**
 
 */
public class PlugInDialogTSPPoint extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	private PlugInAlgorithmTSPPoint TSPPointAlgo = null;
	
	private JTextField pwiImageFileDirectoryText;

	private String pwiImageFileDirectory;
	
	private JTextField baseText;
	
	private JTextField xText;
	
	private int xLoc;
	
	private JTextField yText;
	
	private int yLoc;
	
	private JTextField zText;
	
	private int zLoc;
	
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
	
	private JCheckBox multiThreadingEnabledCheckBox;
	
	private boolean multiThreading;
	
	private JCheckBox correlationCheckBox;
	
	private boolean calculateCorrelation = true;
	
	private String fileNameBase = "IM";
	
	private JCheckBox N4CheckBox;
	
	private boolean doN4MRIBiasFieldCorrection = false;
	
	/**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogTSPPoint() {}

    /**
     * 
     */
    public PlugInDialogTSPPoint(final boolean modal) {
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
        } else if (source == spatialSmoothingCheckBox) {
        	sigmaXLabel.setEnabled(spatialSmoothingCheckBox.isSelected());
        	sigmaXText.setEnabled(spatialSmoothingCheckBox.isSelected());
        	sigmaYLabel.setEnabled(spatialSmoothingCheckBox.isSelected());
        	sigmaYText.setEnabled(spatialSmoothingCheckBox.isSelected());
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
        setTitle("TSP Single Point Analysis");
        
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
        JLabel xLabel = new JLabel("Voxel x location");
        xLabel.setFont(serif12);
        xLabel.setForeground(Color.black);
        inputPanel.add(xLabel, gbc);
        
        gbc.gridx = 1;
        xText = new JTextField(10);
        xText.setText("");
        xText.setFont(serif12);
        xText.setForeground(Color.black);
        inputPanel.add(xText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        JLabel yLabel = new JLabel("Voxel y location");
        yLabel.setFont(serif12);
        yLabel.setForeground(Color.black);
        inputPanel.add(yLabel, gbc);
        
        gbc.gridx = 1;
        yText = new JTextField(10);
        yText.setText("");
        yText.setFont(serif12);
        yText.setForeground(Color.black);
        inputPanel.add(yText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        JLabel zLabel = new JLabel("Voxel z location");
        zLabel.setFont(serif12);
        zLabel.setForeground(Color.black);
        inputPanel.add(zLabel, gbc);
        
        gbc.gridx = 1;
        zText = new JTextField(10);
        zText.setText("");
        zText.setFont(serif12);
        zText.setForeground(Color.black);
        inputPanel.add(zText, gbc);
        
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
        
        gbc.gridy++;
        correlationCheckBox = new JCheckBox("Correlation maps calculated", true);
        correlationCheckBox.setFont(MipavUtil.font12);
        correlationCheckBox.setForeground(Color.black);
        inputPanel.add(correlationCheckBox, gbc);
        
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
        if (algorithm instanceof PlugInAlgorithmTSPPoint) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            //System.out.println("Elapsed: " + algorithm.getElapsedTime());

            if ( (TSPPointAlgo.isCompleted() == true)) {
                insertScriptLine();
            }

            if (TSPPointAlgo != null) {
                TSPPointAlgo.finalize();
                TSPPointAlgo = null;
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

            TSPPointAlgo = new PlugInAlgorithmTSPPoint(pwiImageFileDirectory,
            		xLoc, yLoc, zLoc, spatialSmoothing, sigmax, sigmay, calculateMaskingThreshold, masking_threshold,
            		TSP_threshold, TSP_iter, multiThreading, calculateCorrelation,
            		fileNameBase, doN4MRIBiasFieldCorrection);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            TSPPointAlgo.addListener(this);
            createProgressBar("Creating plugin", " ...", TSPPointAlgo);
            progressBar.setVisible(true);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (TSPPointAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                TSPPointAlgo.run();
            }
        } catch (final OutOfMemoryError x) {
            /*if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }*/

            MipavUtil.displayError("PlugInAlgorithmTSPPoint: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()
    
    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void setGUIFromParams() {
    	pwiImageFileDirectory = scriptParameters.getParams().getString("pwiImageFileDirectory");
    	fileNameBase = scriptParameters.getParams().getString("file_name_base");
        xLoc = scriptParameters.getParams().getInt("x_loc");
        yLoc = scriptParameters.getParams().getInt("y_loc");
        zLoc = scriptParameters.getParams().getInt("z_loc");
        doN4MRIBiasFieldCorrection = scriptParameters.getParams().getBoolean("doN4");
    	spatialSmoothing = scriptParameters.getParams().getBoolean("spatial_smoothing");
    	sigmax = scriptParameters.getParams().getFloat("x_sigma");
    	sigmay = scriptParameters.getParams().getFloat("y_sigma");
    	calculateMaskingThreshold = scriptParameters.getParams().getBoolean("calc_mask_thresh");
    	masking_threshold = scriptParameters.getParams().getInt("mask_thresh");
    	TSP_threshold = scriptParameters.getParams().getDouble("TSP_thresh");
    	TSP_iter = scriptParameters.getParams().getInt("iter");
    	multiThreading = scriptParameters.getParams().getBoolean("multi_thread");
    	calculateCorrelation = scriptParameters.getParams().getBoolean("calc_correlation");
    }
    
    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.getParams().put(ParameterFactory.newParameter("pwiImageFileDirectory", pwiImageFileDirectory));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("file_name_base", fileNameBase));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("x_loc", xLoc));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("y_loc", yLoc));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("z_loc", zLoc));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("doN4", doN4MRIBiasFieldCorrection));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("spatial_smoothing", spatialSmoothing));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("x_sigma", sigmax));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("y_sigma", sigmay));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("calc_mask_thresh", calculateMaskingThreshold));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("mask_thresh", masking_threshold));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("TSP_thresh", TSP_threshold));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("iter", TSP_iter));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("multi_thread", multiThreading));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("calc_correlation", calculateCorrelation));
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
    	
    	tmpStr = xText.getText();
    	try {
    		xLoc = Integer.valueOf(tmpStr).intValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("Voxel x location does not have a proper integer");
    	    xText.requestFocus();
    	    xText.selectAll();
    	    return false;
    	}
    	if (xLoc < 0) {
    		MipavUtil.displayError("Voxel x location must be at least 0");
    		xText.requestFocus();
    	    xText.selectAll();
    	    return false;
    	}
    	
    	tmpStr = yText.getText();
    	try {
    		yLoc = Integer.valueOf(tmpStr).intValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("Voxel y location does not have a proper integer");
    	    yText.requestFocus();
    	    yText.selectAll();
    	    return false;
    	}
    	if (yLoc < 0) {
    		MipavUtil.displayError("Voxel y location must be at least 0");
    		yText.requestFocus();
    	    yText.selectAll();
    	    return false;
    	}
    	
    	tmpStr = zText.getText();
    	try {
    		zLoc = Integer.valueOf(tmpStr).intValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("Voxel z location does not have a proper integer");
    	    zText.requestFocus();
    	    zText.selectAll();
    	    return false;
    	}
    	if (zLoc < 0) {
    		MipavUtil.displayError("Voxel z location must be at least 0");
    		zText.requestFocus();
    	    zText.selectAll();
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
    	
    	
    	multiThreading = multiThreadingEnabledCheckBox.isSelected();
    	
    	calculateCorrelation = correlationCheckBox.isSelected();
    	
    	return true;
    }

}
